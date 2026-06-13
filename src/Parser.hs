module Parser where

import Data.Char
import Control.Applicative
import Data.Either

--           loc  input
type Input = (Int, String)

newtype Parser a = Parser { runParser :: Input -> Either ([String], Input) (a, Input)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (a, restIn) <- p input
    return (f a, restIn)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (x, input) 
  (<*>) (Parser f) (Parser p) = Parser $ \input -> do
    (f, input') <- f input
    (a, input'') <- p input'
    return (f a, input'')

instance Alternative Parser where
  empty = Parser $ \input -> Left (["Failed parser."], input)
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

instance Alternative (Either ([String], Input)) where
  empty = Left ([], (0,""))
  (Right a) <|> _ = Right a 
  (Left  a) <|> (Right b) = Right b
  (Left (e1, (c1, s1))) <|> (Left (e2, (c2, s2))) = if c1 >= c2 -- return the error which parsed the most
                                                            then Left (e1, (c1, s1))
                                                            else Left (e2, (c2, s2))

instance Monad Parser where
    Parser p >>= f = Parser $ \input -> do
                        (a, restIn) <- p input
                        let (Parser np) = f a
                        np restIn

newErr :: String -> Parser a -> Parser a
newErr newError (Parser oldP) = Parser $ \input -> replace $ oldP input
  where replace (Right a) = Right a
        replace (Left (oldErr, a)) = Left (newError:oldErr, a)

replaceErr :: String -> Parser a -> Parser a
replaceErr newError (Parser oldP) = Parser $ \input -> replace $ oldP input
  where replace (Right a) = Right a
        replace (Left (oldErr, a)) = Left ([newError], a)

startParser parser input = runParser parser (0, input)

predicateP :: (Char -> Bool) -> String -> Parser Char
predicateP p err = Parser f
  where
    f (loc, y:ys)
      | p y = Right (y, (locN, ys))
      | otherwise = Left ([err], (locN, y:ys))
      where
        locN = loc + 1
    f (loc, []) = Left ([err ++ ", reached end of input."], (loc, []))

charP :: Char -> Parser Char
charP x = predicateP (x ==) ("Expected " ++ show x)

stringP :: String -> Parser String
stringP input = replaceErr ("Maybe you meant '" ++ input ++ "' ?") $ traverse charP input

spanP :: (Char -> Bool) -> Parser String
spanP predicate = Parser (Right . f)
  where
    f (c, []) = ([], (c, []))
    f (c, x:xs)
      | predicate x = let (ys, (c', zs)) = f (c, xs)
                          c'' = c'+1
                      in (x:ys, (c'', zs))
      | otherwise   = ([], (c, x:xs))

ws :: Parser String
ws = spanP isSpace
wsnn = spanP (==' ')

repeatedParser :: Parser a -> Parser [a]
repeatedParser parser = Parser $ \(c,i) -> if i/=[]
                                             then do
                                               (b, restIn) <- runParser parser (c,i)
                                               (bs, restIn') <- runParser (repeatedParser parser) restIn
                                               return (b:bs, restIn')
                                             else return ([], (c,i))

tryingRepeatedParser :: Parser a -> Parser [a]
tryingRepeatedParser parser = Parser $ \(c,i) -> if i/=[]
                                             then do
                                               let r = runParser parser (c,i)
                                               if isRight r
                                               then do
                                                 let Right (b, restIn) = r
                                                 (bs, restIn') <- runParser (tryingRepeatedParser parser) restIn
                                                 return (b:bs, restIn')
                                               else return ([], (c,i))
                                             else return ([], (c,i))

-- make sure the string parser doesn't change the start of the input
-- this will lead to incorrect error reporting
(>>>) :: Parser String -> Parser b -> Parser b
f >>> g = Parser $ \input -> do
            (s, restIn) <- runParser f input
            let (c, i) = input
            let a = runParser g (c,s)
            case a of
              Right (r, (c', i')) -> if null i'
                                         then Right (r, restIn)
                                         else Left (["Unexpected string, "++i'], (c', i))
              Left (err, (c', i'))  -> Left (err, (c', i))

ignoreErrorIndex p = Parser $ \input -> do
                             let o = runParser p input
                             if isLeft o then (\(Left (err, (loc, s))) -> Left (err, (fst input, s))) o else o
