{-# LANGUAGE OverloadedStrings #-}
module Parser where

import           Control.Applicative
import qualified Data.Text as T
import           Data.Either
import           Data.Char

--           loc  input
type Input = (Int, T.Text)

newtype Parser a = Parser { runParser :: Input -> Either ParserError (a, Input)}

data ParserError = Failure [T.Text] Input
                 | Error T.Text Input
                   deriving (Show)

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
    empty = Parser $ \input -> Left (Failure ["Failed parser."] input)
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

instance Alternative (Either ParserError) where
    empty = Left (Failure [] (0,"UNREACHABLE"))
    (Right a) <|> _ = Right a
    (Left  (Failure a e)) <|> (Right b) = Right b
    (Left (Error e i)) <|> _ = Left (Error e i)
    _ <|> (Left (Error e i)) = Left (Error e i)
    (Left (Failure e1 (c1, s1))) <|> (Left (Failure e2 (c2, s2))) = if c1 >= c2 -- return the failure which parsed the most
                                                                    then Left (Failure e1 (c1, s1))
                                                                    else Left (Failure e2 (c2, s2))

instance Monad Parser where
    Parser p >>= f = Parser $ \input -> do
        (a, restIn) <- p input
        let (Parser np) = f a
        np restIn

newErr :: T.Text -> Parser a -> Parser a
newErr newError (Parser oldP) = Parser $ \input -> replace $ oldP input
  where replace (Right a)                 = Right a
        replace (Left (Failure oldErr a)) = Left (Failure (newError:oldErr) a)
        replace (Left (Error s i))        = Left (Error s i)

replaceErr :: T.Text -> Parser a -> Parser a
replaceErr newError (Parser oldP) = Parser $ \input -> replace $ oldP input
  where replace (Right a)                 = Right a
        replace (Left (Failure oldErr a)) = Left (Failure [newError] a)
        replace (Left (Error s i))        = Left (Error s i)

failureToError :: T.Text -> Parser a -> Parser a
failureToError newError (Parser oldP) = Parser $ \input -> replace $ oldP input
    where replace (Right a) = Right a
          replace (Left (Failure oldErr a)) = Left (Error (T.unlines $ newError:oldErr) a)
          replace (Left (Error oldErr a)) = Left (Error oldErr a)

-- NOTE: Be careful when using this to make monadic parsers, you will have to
--       manage location manually, as this just uses 0 location as default
startParser :: (Parser a) -> T.Text -> Either ParserError (a, Input)
startParser parser input = runParser parser (0, input)

predicateP :: (Char -> Bool) -> T.Text -> Parser Char
predicateP p err = Parser f
  where
    f (loc, w) = if T.null w
                 then Left (Failure [T.append err ", reached end of input."] (loc, T.empty))
                 else let locN = loc + 1
                          y = T.head w
                          ys = T.tail w
                      in if p y
                         then Right (y, (locN, ys))
                         else Left (Failure [err] (locN, y `T.cons` ys))

charP :: Char -> Parser Char
charP x = predicateP (x ==) (T.append "Expected " $ T.pack $ show x)

stringP :: T.Text -> Parser T.Text
stringP input = replaceErr ("Maybe you meant '" `T.append` input `T.append` "' ?") $ bTraverse charP input

bTraverse :: (Char -> Parser Char) -> T.Text -> Parser T.Text
bTraverse cp input = if T.null input
                     then return T.empty
                     else T.cons <$> cp (T.head input) <*> bTraverse cp (T.tail input)

spanP :: (Char -> Bool) -> Parser T.Text
spanP predicate = Parser (Right . f)
  where
    f (c, w) = if T.null w
               then (T.empty, (c, T.empty))
               else let x  = T.head w
                        xs = T.tail w
                    in if predicate x
                       then let (ys, (c', zs)) = f (c, xs)
                                c'' = c'+1
                            in (x `T.cons` ys, (c'', zs))
                       else (T.empty, (c, x `T.cons` xs))

ws :: Parser T.Text
ws = spanP isSpace
wsnn = spanP (==' ')

repeatedParser :: Parser a -> Parser [a]
repeatedParser parser = Parser $ \(c,i) -> if i/=T.empty
                                             then do
                                               (b, restIn) <- runParser parser (c,i)
                                               (bs, restIn') <- runParser (repeatedParser parser) restIn
                                               return (b:bs, restIn')
                                             else return ([], (c,i))

tryingRepeatedParser :: Parser a -> Parser [a]
tryingRepeatedParser parser = Parser $ \(c,i) -> if i/=T.empty
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
(>>>) :: Parser T.Text -> Parser b -> Parser b
f >>> g = Parser $ \input -> do
            (s, restIn) <- runParser f input
            let (c, i) = input
            let a = runParser g (c,s)
            case a of
              Right (r, (c', i')) -> if T.null i'
                                         then Right (r, restIn)
                                         else Left (Failure [T.append "Unexpected string, " i'] (c', i))
              Left (Failure err (c', i'))  -> Left (Failure err (c', i))
              Left (Error e (c', i')) -> Left (Error e (c', i))

ignoreErrorIndex p = Parser $ \input -> do
                             let o = runParser p input
                             if isLeft o
                             then case o of
                                    (Left (Failure err (loc, s))) -> Left (Failure err (fst input, s))
                                    (Left (Error e i)) -> Left (Error e i)
                             else o
