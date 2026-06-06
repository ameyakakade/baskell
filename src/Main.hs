module Main where

import Parser
import BParser
import Generator
import TargetGasAArch64MacOS
import System.Environment

main :: IO ()
main = do
  (fileName:_) <- getArgs
  a <- readFile fileName
  let parsed = startParser bProgram a
  let (Right (r,_)) = parsed
  let irp = gProgram r
  let asmo = asm (snd irp)
  writeFile "as.s" asmo

compileFile :: String -> IO ()
compileFile fileName = do
  a <- readFile fileName
  let parsed = startParser bProgram a
  putStrLn "\nAST:"
  prettyier parsed
  let (Right (r,_)) = parsed
  let irp = gProgram r
  putStrLn "\nIR:"
  prettyier irp
  putStrLn "\nASM:"
  let asmo = asm (snd irp)
  putStrLn asmo
  writeFile "as.s" asmo
