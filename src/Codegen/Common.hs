module Codegen.Common where

import Generator
import Data.List
import Data.Maybe
import Control.Monad
import Data.Time.Clock
import System.Directory
import System.Exit
import Parser
import BParser

data Target = Target {
    targetName          :: String,
    targetBuildRecipe   :: Bool -> String -> [String] -> [String] -> [String] -> IO ()
    --                 Recompile   Output    Src files   Obj files   Linker Flags
    }

newtype StateM state b = StateM { runStateM :: state -> (state, b) } deriving (Functor)

instance Applicative (StateM a) where
    pure a = StateM (,a)
    (StateM x) <*> (StateM y) = StateM $ \c -> let (cs, f) = x c
                                                   (cs',t) = y cs
                                               in (cs', f t)

instance Monad (StateM a) where
    x >>= y = StateM $ \c -> let (cs, input) = runStateM x c
                              in runStateM (y input) cs

bd = ".baskellbuild/"

getFileName :: String -> FilePath -> FilePath
getFileName ext fp = bd ++ takeWhile (/='.') fp ++ ext

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

prettyProcess :: Show a => IO (a, String, String) -> IO ()
prettyProcess p = do
    (exit, stdout, stderr) <- p
    print exit
    putStr stdout
    putStr stderr

compileFile :: (IRProgram -> IO String) -> Bool -> String -> IO ()
compileFile target dumpInfo fileName = do
    a <- readFile fileName
    let newLines = map snd $ filter (\(x,_) -> x=='\n') $ zip a [0..]
    let parsed = runParser bProgram a
    let (_,r) = parsed
    pure ()

{-
    case parsed of
      (Right (r,_)) ->
          do
           -- when dumpInfo (do
           --                 putStrLn "\nAST:"
           --                 prettyier parsed)
            let irp = gProgram r
            when dumpInfo (do
                            putStrLn "\nIR:"
                            prettyier $ functions $ snd irp
                            -- prettyier $ nakedFunctions $ snd irp
                            -- prettyier $ globalVars $ snd irp    
                            prettyier $ extrns $ snd irp
                            -- prettyier $ variadics $ snd irp
                          )
            asmo <- target (snd irp)
            -- when dumpInfo (do
            --       putStrLn "\nASM:"
            --       putStrLn asmo)
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
-}
findLoc :: [Int] -> Int -> String
findLoc ns loc' = show (length n + 1) ++ ":" ++ show (loc-last (0:n)) ++ ":"
  where n = filter (<loc) ns
        loc = loc' - 1

findLocLen :: [Int] -> (Int,Int) -> String
findLocLen ns (loc,len) = show (length n + 1) ++ ":" ++ show (loc-last (0:n)) ++ suff
  where n = filter (<loc) ns
        suff = if len==0 then ":"
               else "-" ++ show (loc-last (0:n) + len - 1) ++ ":"
