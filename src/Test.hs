-- This contains minimal json parser and other utils for running test suite

module Main where
import Parser
import Generator(prettyier)

import Control.Applicative
import Control.Monad
import Data.Either
import Data.Maybe
import Data.List
import System.Directory
import System.Process
import System.Exit

data JsonValue = JsonNull
               | JsonObject [(String, JsonValue)]
               | JsonString String
               | JsonArray [JsonValue]
                 deriving (Show, Eq)

main = do
  json <- startParser jsonValue <$> readFile "../thirdparty/tests.json"
  when (isLeft json)
           (do
             error "Error in JSON.")
          
  let (Right (JsonArray jsonTree, _)) = json
  let filteredJsonTree = filter
                         (\(JsonObject maps) -> let Just (_,JsonString target) = find (\(s,_) -> s=="target") maps
                                                in target=="gas-aarch64-darwin")
                         jsonTree
                         
  -- prettyier filteredJsonTree
  doesFileExist "./baskell" >>= (\x -> when x (error "Compiler executable not found.")) . not
  setCurrentDirectory "../thirdparty/tests/"
  ls <- traverse testCase filteredJsonTree
  putStrLn ""
  putStrLn $ "Passed:   " ++ (show $ length $ filter (==0) ls)
  putStrLn $ "Failed:   " ++ (show $ length $ filter (==2) ls)
  putStrLn $ "Error:    " ++ (show $ length $ filter (==1) ls)
  putStrLn $ "Disabled: " ++ (show $ length $ filter (==3) ls)
  return ()

testCase :: JsonValue -> IO Int 
testCase (JsonObject maps) = do
  putStrLn ""
  let caseName = getJsonValue "case"
  let fileName = caseName ++ ".b"
  doesFileExist fileName >>= (\x -> when x (putStrLn $ "ERROR: File \"" ++ fileName ++ "\" not found.")) . not
  if (getJsonValue "state")=="Disabled"
  then do
    putStrLn $ "Test " ++ fileName ++ " is disabled"
    return 3
  else do
    let comment = getJsonValue "comment"
    when (not $ null comment) $ putStrLn $ "Comment: " ++ comment
    (exit, stdout, stderr) <- readProcessWithExitCode "../../src/baskell" [fileName] ""
    fe <- doesFileExist caseName
    if exit==(ExitSuccess) && fe
    then do
      putStrLn $ "Compiled " ++ fileName ++ " successfully."
      (exit, stdout, stderr) <- readProcessWithExitCode ("./"++caseName) [] ""
      let expectedOut = getJsonValue "expected_stdout"
      if expectedOut==stdout
      then do
        putStrLn "Test passed successfully :)"
        return 0
      else do
        putStrLn $ "FAILED: Couldn't match output " ++ (show stdout) ++ " with expected output " ++ (show expectedOut)
        return 2
    else do
      putStrLn $ "ERROR: Could not compile " ++ fileName ++ "."
      putStr stdout
      putStr stderr
      return 1

    where getJsonValue key = let Just (_, JsonString value) = find (\(s,_) -> s==key) maps
                             in value

jsonValue :: Parser JsonValue
jsonValue = jsonString <|> jsonObject <|> jsonArray

jsonString :: Parser JsonValue
jsonString = fmap JsonString $ ws *> charP '\"' *> escapedStringP (/='\"') <* charP '\"'

jsonObject :: Parser JsonValue
jsonObject = fmap JsonObject $ charP '{' *> ws *> ((:) <$> sp <*> tryingRepeatedParser (charP ',' *> ws *> sp)) <* charP '}'
    where sp = (,) <$> (fmap (\(JsonString s) -> s) jsonString <* ws) <*> (charP ':' *> jsonValue <* ws)

jsonArray :: Parser JsonValue
jsonArray = fmap JsonArray $ charP '[' *> ws *> ((:) <$> jsonObject <* ws <*> tryingRepeatedParser (charP ',' *> ws *> jsonObject <* ws)) <* ws <* charP ']'

escapedStringP :: (Char -> Bool) -> Parser String
escapedStringP predicate = Parser f
  where
    f (c, []) = Right ([], (c, []))
    f (c, x:xs)
      | x=='\\' = let (a:as) = xs
                      a' = escapedChars a
                  in if isNothing a'
                     then Left (Error "Invalid escape char" (c, xs))
                     else let b = f (c, as)
                          in if isRight b
                             then let Right (ys, (c', zs)) = b
                                      c'' = c'+2
                                  in Right (fromJust a':ys, (c'', zs))
                             else b
      | predicate x = let a = f (c, xs)
                      in if isRight a
                         then let Right (ys, (c', zs)) = a
                                  c'' = c'+1
                              in Right (x:ys, (c'', zs))
                         else a
      | otherwise   = Right ([], (c, x:xs))
    escapedChars c = case c of
                       '"' -> Just '\"'
                       '\\' -> Just '\\'
                       '/' -> Just '/'
                       'b' -> Just '\b'
                       'f' -> Just '\f'
                       'n' -> Just '\n'
                       'r' -> Just '\r'
                       't' -> Just '\t'
                       a   -> Nothing
