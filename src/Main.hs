module Main where

import Parser
import BParser
import Generator
import TargetGasAArch64MacOS

import Control.Monad
import System.Environment
import System.Process
import System.Directory
import Data.Maybe
import Data.List
import Data.Foldable
import Data.Time.Clock

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

  let compilerDir = "/Users/ameya/Documents/Programming/baskell/src/"
  cdir <- getCurrentDirectory
  setCurrentDirectory compilerDir

  newC <- runIfChanged False ["Main.hs", "Parser.hs", "BParser.hs", "Generator.hs", "TargetGasAArch64MacOS.hs"] "baskell"
          (prettyProcess $ readProcessWithExitCode "/Users/ameya/.ghcup/bin/ghc" ["-o", "baskell", "Main.hs"] "")

  setCurrentDirectory cdir

  let std = compilerDir ++ "write.o"

  let nC = isJust $ find (=="-B") args
  let sourceFiles = filter (isSuffixOf ".b") args
  objectFiles <- traverse makeAbsolute $ filter (isSuffixOf ".o") args

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
                                     (getFileName ".as" fileName)
                                     (compileFile False fileName)

                    runIfChanged nC [getFileName ".as" fileName]
                                     (getFileName ".o" fileName)
                                     (prettyProcess $ readProcessWithExitCode "as"
                                                        ["-arch", "arm64", "-o", getFileName ".o" fileName, getFileName ".as" fileName] "")
                  ) sourceFiles

         runIfChanged nC (objectFiles ++ (std:map (getFileName ".o") sourceFiles))
            (takeWhile (/='.') fileName)
            (prettyProcess $ readProcessWithExitCode "gcc" (["-o", takeWhile (/='.') fileName, std] ++ map (getFileName ".o") sourceFiles ++ objectFiles) "")
         return ()

prettyProcess :: Show a => IO (a, String, String) -> IO ()
prettyProcess p = do
  (exit, stdin, stderr) <- p
  print exit
  putStr stdin
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
          if null (fst irp)
          then do
            writeFile (getFileName ".as" fileName) asmo
            putStrLn "Compiled successfully"
          else do
            putStr $ unlines $ map (\e -> if isNothing $ genErrorLocLength e
                                          then fileName ++ ":" ++ "\t" ++ "ERROR: " ++ genErrorString e
                                          else fileName ++ ":" ++ findLocLen newLines (fromJust $ genErrorLocLength e) ++ "\t" ++ "ERROR: " ++ genErrorString e)
                       (fst irp)
            putStrLn $ "Could not compile due to " ++ show (length $ fst irp) ++ " errors."
            putStrLn ""

          when dumpInfo (do
                putStrLn "\nASM:"
                putStrLn asmo)

    (Left (errors,(loc, s))) -> do
                putStrLn "Syntax error"
                putStr $ fileName ++ ":"
                putStrLn $ findLoc newLines loc
                putStr $ unlines errors

findLoc :: [Int] -> Int -> String
findLoc ns loc' = show (length n + 1) ++ ":" ++ show (loc-last (0:n)) ++ ":"
    where n = filter (<loc) ns
          loc = loc' - 1

findLocLen :: [Int] -> (Int,Int) -> String
findLocLen ns (loc,len) = show (length n + 1) ++ ":" ++ show (loc-last (0:n)) ++ "-" ++ show (loc-last (0:n) + len - 1) ++ ":"
    where n = filter (<loc) ns
