module Main where

import BParser
import Generator
import Parser
import TargetGasAArch64MacOS

import Control.Monad
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Time.Clock
import System.Directory
import System.Environment
import System.Exit
import System.Process

bd = ".baskellbuild/"

getFileName :: String -> FilePath -> FilePath
getFileName ext fp = bd ++ takeWhile (/='.') fp ++ ext

setDir :: FilePath -> IO FilePath
setDir fileP = do
  let (fn, dir) = span (/='/') $ reverse fileP
  if null dir
  then setCurrentDirectory "."
  else setCurrentDirectory (reverse dir)
  return (reverse fn)

main :: IO ()
main = do
  args <- getArgs

  let std = "/Users/ameya/Documents/Programming/baskell/build/" ++ "write.o"
  let newC = False

  let nC = isJust $ find (=="-B") args
  let sourceFiles = filter (isSuffixOf ".b") args
  objectFiles <- traverse makeAbsolute $ filter (\x -> isSuffixOf ".o" x || isSuffixOf ".a" x) args
  let linkerFlags = drop 1 $ dropWhile (/="-L") args

  if null args then putStrLn "No input files."
  else if newC
       then do
         putStrLn "Possibly updated compiler. Use flag -B to rebuild everything"
       else do
         let (fileDirName:_) = sourceFiles
         fileName <- setDir fileDirName
         createDirectoryIfMissing False bd

         traverse_ (\fileName -> do
                    runIfChanged nC [fileName]
                                     (getFileName ".s" fileName)
                                     (compileFile False fileName)

                    runIfChanged nC [getFileName ".s" fileName]
                                     (getFileName ".o" fileName)
                                     (prettyProcess $ readProcessWithExitCode "as"
                                                        ["-arch", "arm64", "-o", getFileName ".o" fileName, getFileName ".s" fileName] "")
                  ) sourceFiles

         runIfChanged nC (objectFiles ++ (std:map (getFileName ".o") sourceFiles))
            (takeWhile (/='.') fileName)
            (prettyProcess $ readProcessWithExitCode "gcc" (["-o", takeWhile (/='.') fileName, std] ++ map (getFileName ".o") sourceFiles ++ objectFiles ++ linkerFlags) "")
         return ()

prettyProcess :: Show a => IO (a, String, String) -> IO ()
prettyProcess p = do
  (exit, stdout, stderr) <- p
  print exit
  putStr stdout
  putStr stderr

runIfChanged :: Show a => Bool -> [FilePath] -> FilePath -> IO a -> IO Bool
runIfChanged force fp out ting = do
  t <- getCurrentTime
  cs <- traverse (checkChange out) fp
  if or cs || force
  then do
    putStrLn $ "Making " ++ out ++ " from " ++ intercalate ", " fp
    ting
    putStrLn ""
    return True
  else do
    return False

checkChange :: FilePath -> FilePath -> IO Bool
checkChange out fp = do
  inT <- getModificationTime fp
  fileExists <- doesFileExist out
  if fileExists
  then do
    outT <- getModificationTime out
    let d = diffUTCTime inT outT
    return $ d > 0
  else return True

compileFile :: Bool -> String -> IO ()
compileFile dumpInfo fileName = do
  a <- readFile fileName
  let newLines = map snd $ filter (\(x,_) -> x=='\n') $ zip a [0..]
  let parsed = startParser bProgram a
  let (Right (r,_)) = parsed

  case parsed of
    (Right (r,_)) ->
        do
          when dumpInfo (do
                          putStrLn "\nAST:"
                          prettyier parsed)
          let irp = gProgram r
          when dumpInfo (do
                          putStrLn "\nIR:"
                          prettyier irp)
          let asmo = asm (snd irp)
          when dumpInfo (do
                putStrLn "\nASM:"
                putStrLn asmo)
          if null (fst irp)
          then do
            writeFile (getFileName ".s" fileName) asmo
            putStrLn "Compiled successfully"
          else do
            putStr $ unlines $ map (\e -> if isNothing $ genErrorLocLength e
                                          then fileName ++ ":" ++ "\t" ++ "ERROR: " ++ genErrorString e
                                          else fileName ++ ":" ++ findLocLen newLines (fromJust $ genErrorLocLength e) ++ "\t" ++ "ERROR: " ++ genErrorString e)
                       (fst irp)
            putStrLn $ "Could not compile due to " ++ show (length $ fst irp) ++ " errors."
            putStrLn ""
            exitWith (ExitFailure 1)

    (Left (Failure errors (loc, s))) -> do
                putStrLn "Syntax failure"
                putStr $ fileName ++ ":"
                putStrLn $ findLoc newLines loc
                putStr $ unlines errors
                exitWith (ExitFailure 1)

    (Left (Error error (loc, s))) -> do
                putStrLn "Syntax error"
                putStr $ fileName ++ ":"
                putStrLn $ findLoc newLines loc
                putStr error
                exitWith (ExitFailure 1)

findLoc :: [Int] -> Int -> String
findLoc ns loc' = show (length n + 1) ++ ":" ++ show (loc-last (0:n)) ++ ":"
    where n = filter (<loc) ns
          loc = loc' - 1

findLocLen :: [Int] -> (Int,Int) -> String
findLocLen ns (loc,len) = show (length n + 1) ++ ":" ++ show (loc-last (0:n)) ++ "-" ++ show (loc-last (0:n) + len - 1) ++ ":"
    where n = filter (<loc) ns
