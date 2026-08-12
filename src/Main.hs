module Main where

import BParser
import Codegen
import Generator
import Parser

import Control.Monad
import Codegen.GasAArch64
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Time.Clock
import System.Directory
import System.Environment
import System.Exit
import System.Process

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

    let newC = False

    let nC = isJust $ find (=="-B") args
    let targetName = let a = drop 2 <$> find (isPrefixOf "-T") args
                     in fromMaybe "gasAArch64" a
    let (Just target) = find (\(Target s _) -> s == targetName) targets
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
        targetBuildRecipe target nC fileName sourceFiles objectFiles linkerFlags
