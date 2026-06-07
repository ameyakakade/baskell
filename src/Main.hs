module Main where

import Parser
import BParser
import Generator
import TargetGasAArch64MacOS

import Control.Monad
import System.Environment
import Data.Maybe

main :: IO ()
main = do
  (fileName:_) <- getArgs
  compileFile False fileName

compileFile :: Bool -> String -> IO ()
compileFile dumpInfo fileName = do
  putStrLn ""
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
            writeFile "as.s" asmo
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
                putStrLn $ findLoc newLines (loc)
                putStr $ unlines errors

findLoc :: [Int] -> Int -> String
findLoc ns loc' = show (length n + 1) ++ ":" ++ show (loc-last (0:n)) ++ ":"
    where n = filter (<loc) ns
          loc = loc' - 1

findLocLen :: [Int] -> (Int,Int) -> String
findLocLen ns (loc,len) = show (length n + 1) ++ ":" ++ show (loc-last (0:n)) ++ "-" ++ show (loc-last (0:n) + len - 1) ++ ":"
    where n = filter (<loc) ns
