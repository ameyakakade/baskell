module Main where

import Parser
import BParser
import Generator

main :: IO ()
main = putStrLn "Hello Sailor"

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
