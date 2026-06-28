import Data.List
import Data.Time.Clock
import System.Directory
import System.Environment
import System.Process

main :: IO ()
main = do
  ck <- doesFileExist "src/Main.hs"
  if ck then return () else error "Could not find file \"src/Main.hs\""
  createDirectoryIfMissing False "build/"
  logProcess $ readProcessWithExitCode "ghc" ["-o", "build/baskell", "-outputdirbuild", "src/Main.hs", "-isrc/"] ""
  runIfChanged False ["src/Test.hs"] "build/btest"
    $ logProcess
    $ readProcessWithExitCode "ghc" ["-o", "build/btest", "-outputdirbuild", "src/Test.hs", "-isrc/"] ""
  runIfChanged False ["src/write.c"] "build/write.o"
    $ logProcess
    $ readProcessWithExitCode "cc" ["-c", "src/write.c", "-o", "build/write.o"] ""

logProcess p = do
  (e,so,se) <- p
  putStr so
  putStr se

runIfChanged :: Bool -> [FilePath] -> FilePath -> IO () -> IO ()
runIfChanged force fp out ting = do
  t <- getCurrentTime
  cs <- traverse (checkChange out) fp
  if or cs || force
  then do
    putStrLn $ "Making " ++ out ++ " from " ++ intercalate ", " fp
    ting
    putStrLn ""
  else do
    return ()

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
