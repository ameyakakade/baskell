module Parser where

import Data.Char
import Control.Applicative

--           line  col  input
type Input = (Int, Int, String)

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
  empty = Left ([], (0,0,""))
  (Right a) <|> _ = Right a 
  (Left  a) <|> (Right b) = Right b
  (Left (e1, (l1, c1, s1))) <|> (Left (e2, (l2, c2, s2))) = if (l1+c1) >= (l2+c2) -- return the error which parsed the most
                                                            then Left (e1, (l1, c1, s1))
                                                            else Left (e2, (l2, c2, s2))

instance Monad Parser where
    Parser p >>= f = Parser $ \input -> do
                        (a, restIn) <- p input
                        let (Parser np) = f a
                        np restIn
                        
newErr :: String -> Parser a -> Parser a
newErr newError (Parser oldP) = Parser $ \input -> replace $ oldP input
  where replace (Right a) = Right a
        replace (Left (oldErr, a)) = Left (newError:oldErr, a)

startParser parser input = runParser parser (0, 0, input)

getNewIndex :: Int -> Int -> Char -> (Int, Int)
getNewIndex line col char = (lineN, colN)
  where
    colN = if isNewLine then 0 else col+1
    lineN = (if isNewLine then 1 else 0) + line
    isNewLine = char == '\n'

predicateP :: (Char -> Bool) -> String -> Parser Char
predicateP p err = Parser f
  where
    f (line, col, y:ys)
      | p y = Right (y, (lineN, colN, ys))
      | otherwise = Left ([err], (lineN, colN, y:ys))
      where
        (lineN, colN) = getNewIndex line col y
    f (line, col, []) = Left ([err ++ ", reached end of input."], (line, col, []))

charP :: Char -> Parser Char
charP x = predicateP (x ==) ("Expected " ++ show x)

stringP :: String -> Parser String
stringP input = newErr ("Expected " ++ input) $ traverse charP input

spanP :: (Char -> Bool) -> Parser String
spanP predicate = Parser (Right . f)
  where
    f (c, r, []) = ([], (c, r, []))
    f (l, c, x:xs)
      | predicate x = let (ys, (l', c', zs)) = f (l, c, xs)
                          (l'', c'') = getNewIndex l' c' x
                      in (x:ys, (l'', c'', zs))
      | otherwise   = ([], (l, c, x:xs))

ws :: Parser String
ws = spanP isSpace

-- this function selects a string surrounded by brackets.
-- it even works for nested brackets
selectBracketed :: Char -> Char -> Int -> Parser String
selectBracketed sI eI n = (charP eI <|> charP sI) >>= f
    where p c = c /= sI && c /= eI
          f b = Parser $ \i ->
                let z bs ns = do
                      (s, restIn) <- runParser (spanP p) i
                      (a, restIn') <- runParser (selectBracketed sI eI ns) restIn
                      Right (bs++s++a, restIn')
                in
                  if b==eI
                  then if n == 1
                       then Right ([], i)
                       else z [b] (n-1)
                  else if n == 0
                       then z [] (n+1)
                       else z [b] (n+1)

repeatedParser :: Parser a -> Parser [a]
repeatedParser parser = Parser $ \(l,r,i) -> if i/=[]
                                             then do
                                               (b, restIn) <- runParser parser (l,r,i)
                                               (bs, restIn') <- runParser (repeatedParser parser) restIn
                                               return (b:bs, restIn')
                                             else return ([], (l,r,i))

(>>>) :: Parser String -> Parser b -> Parser b
f >>> g = Parser $ \input -> do
            (s, restIn) <- runParser f input
            let (l, c, i) = input
            let a = startParser g s
            case a of
              Right (r, (l', c', i')) -> if null i'
                                         then Right (r, restIn)
                                         else Left (["Unexpected string, "++i'], (l+l', c+c', i))
              Left (err, (l', c', i'))  -> Left (err, (l+l', c+c', i))
