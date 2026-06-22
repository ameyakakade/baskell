-- This contains minimal json parser and other utils for running test suite

module Test where
import Parser

import Control.Applicative
import Data.Either
import Data.Maybe

data JsonValue = JsonNull
               | JsonObject [(String, JsonValue)]
               | JsonString String
               | JsonArray [JsonValue]
                 deriving (Show, Eq)

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
