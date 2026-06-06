module Main where

import Parser
import BParser
import Generator
import TargetGasAArch64MacOS

import Control.Monad
import System.Environment

main :: IO ()
main = do
  (fileName:_) <- getArgs
  compileFile False fileName

compileFile :: Bool -> String -> IO ()
compileFile dumpInfo fileName = do
  a <- readFile fileName
  let newLines = map snd $ filter (\(x,_) -> x=='\n') $ zip a [0..]
  let parsed = startParser bProgram a
  let (Right (r,_)) = parsed

  case parsed of
    (Right (r,_)) -> do
                let irp = gProgram r
                let asmo = asm (snd irp)
                writeFile "as.s" asmo
                let irp = gProgram r
                let asmo = asm (snd irp)
                writeFile "as.s" asmo
                when dumpInfo
                 (do
                  putStrLn "\nAST:"
                  prettyier parsed
                  putStrLn "\nIR:"
                  prettyier irp
                  putStrLn "\nASM:"
                  putStrLn asmo)

    (Left (errors,(loc, s))) -> do
                putStrLn "Syntax error"
                putStr $ fileName ++ ":"
                putStrLn $ findLoc newLines (loc-1)
                putStr $ unlines errors

findLoc :: [Int] -> Int -> String
findLoc ns loc = show (length n + 1) ++ ":" ++ show (loc-last (0:n)) ++ ":"
    where n = filter (<loc) ns
