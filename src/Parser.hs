module Parser where

import Data.Char
import Control.Applicative

--           line  col  input
type Input = (Int, Int, String)

data BProgram = Program [BDefinition]
              deriving (Eq, Show)

data BDefinition = BDefinition
                 deriving (Eq, Show)

data BIVal = IConstant BConstant
           | IName BName
           deriving (Eq, Show)

data BStatement = Auto     [(BName, BConstant)]
                | Extrn    [BName]
                | Default  [(BConstant, BStatement)] -- TODO: Confirm if this correct
                | Case     [(BConstant, BStatement)]
                | Block    [BStatement]
                | IfElse   (BRValue, BStatement, BStatement)
                | While    (BRValue, BStatement)
                | Switch   (BRValue, BStatement)
                | Goto     BRValue
                | Return   BRValue
                | SRValue  BRValue
                deriving (Eq, Show)

data BRValue = BracketRValue BRValue
             | RLValue       BLValue
             | RConstant     BConstant
             | Assignment    (BLValue, BRValue)
             | IncDecPre     (BIncDec, BLValue)
             | IncDecPost    (BIncDec, BLValue)
             | RUnary        (BUnary, BRValue)
             | GetAddress    BLValue
             | Binary        (BRValue, BBinary, BRValue)
             | Ternary       (BRValue, BRValue, BRValue)
             -- TODO
             deriving (Eq, Show)

data BAssign = Assign
             | BinaryAssign BBinary
             deriving (Eq, Show)

data BIncDec = Increment
             | Decrement
             deriving (Eq, Show)

data BUnary = Negative
            | Exclamation
            deriving (Eq, Show)

data BBinary = Or
             | And
             | Equal
             | NotEqual
             | LessThan
             | LessThanOrEqual
             | MoreThan
             | MoreThanOrEqual
             | ShiftLeft
             | ShiftRight
             | Add
             | Subtract
             | Modulo
             | Multiply
             | Divide
             deriving (Eq, Show)

data BLValue = LName       BName
             | Dereference BRValue
             | Array       (BRValue, BRValue)     -- TODO: Confirm if this is correct
             deriving (Eq, Show)

data BConstant = Digit Int
               | Char Char
               | Chars String
               deriving (Eq, Show)

data BName = Name String
           deriving (Eq, Show)
--                                                         Errors   input 
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
  
newErr :: String -> Parser a -> Parser a
newErr newError (Parser oldP) = Parser $ \input -> replace $ oldP input
  where replace (Right a) = (Right a)
        replace (Left (oldErr, a)) = (Left (newError:oldErr, a))

getNewIndex :: Int -> Int -> Char -> (Int, Int)
getNewIndex line col char = (lineN, colN)
  where
    colN = if isNewLine then 0 else (col+1)
    lineN = (if isNewLine then 1 else 0) + line
    isNewLine = char == '\n'

predicateP :: (Char -> Bool) -> String -> Parser Char
predicateP p err = Parser f
  where
    f (line, col, (y:ys))
      | p y = Right (y, (lineN, colN, ys))
      | otherwise = Left ((err):[], (lineN, colN, y:ys))
      where
        (lineN, colN) = getNewIndex line col y
    f (line, col, []) = Left ((err ++ ", reached end of file."):[], (line, col, []))

charP :: Char -> Parser Char
charP x = predicateP ((==) x) ("Expected " ++ (show x))

stringP :: String -> Parser String
stringP input = newErr ("Expected " ++ input) $ sequenceA $ map charP input

spanP :: (Char -> Bool) -> Parser String
spanP predicate = Parser (\input -> Right (f input))
  where
    f (c, r, []) = ([], (c, r, []))
    f (l, c, x:xs)
      | predicate x = let (ys, (l', c', zs)) = f (l, c, xs)
                          (l'', c'') = getNewIndex l' c' x
                      in (x:ys, (l'', c'', zs))
      | otherwise   = ([], (l, c, x:xs))

ws :: Parser String
ws = spanP isSpace

bIVal :: Parser BIVal
bIVal = (fmap IConstant $ bConstant)
        <|> (fmap IName $ bName)

bAssign :: Parser BAssign
bAssign = (fmap BinaryAssign $ charP '=' *> bBinary)
          <|> (fmap (\_ -> Assign) $ charP '=')

bIncDec :: Parser BIncDec
bIncDec = newErr "Expected increment or decrement" $
          (fmap (\_ -> Increment) $ stringP "++")
          <|> (fmap (\_ -> Decrement) $ stringP "--")

bUnary :: Parser BUnary
bUnary = newErr "Expected a unary operator"
          (fmap (\_ -> Negative) $ charP '-')
          <|> (fmap (\_ -> Exclamation) $ charP '!')

bBinary :: Parser BBinary
bBinary = newErr "Expected a binary operator" $
          (fmap (\_ -> Or) $ stringP "|")
          <|> (fmap (\_ -> And) $ stringP "&")
          <|> (fmap (\_ -> Equal) $ stringP "==")
          <|> (fmap (\_ -> NotEqual) $ stringP "!=")
          <|> (fmap (\_ -> LessThan) $ stringP "<")
          <|> (fmap (\_ -> LessThanOrEqual) $ stringP "<=")
          <|> (fmap (\_ -> MoreThan) $ stringP ">")
          <|> (fmap (\_ -> MoreThanOrEqual) $ stringP ">=")
          <|> (fmap (\_ -> ShiftLeft) $ stringP "<<")
          <|> (fmap (\_ -> ShiftRight) $ stringP ">>")
          <|> (fmap (\_ -> LessThanOrEqual) $ stringP "<=")
          <|> (fmap (\_ -> LessThan) $ stringP "<")
          <|> (fmap (\_ -> MoreThanOrEqual) $ stringP ">=")
          <|> (fmap (\_ -> MoreThan) $ stringP ">")
          <|> (fmap (\_ -> Add) $ stringP "+")
          <|> (fmap (\_ -> Subtract) $ stringP "-")
          <|> (fmap (\_ -> Modulo) $ stringP "%")
          <|> (fmap (\_ -> Multiply) $ stringP "*")
          <|> (fmap (\_ -> Divide) $ stringP "/")

bConstant :: Parser BConstant
bConstant = (fmap Digit $ fmap read $ fmap (:) (predicateP isDigit "Expected atleast one digit") <*> (spanP isDigit) )
            <|> (fmap Char  $ charP '`' *> predicateP isAlpha "Expected a character" <* charP '`')
            <|> (fmap Chars $ charP '"' *> spanP (/='"') <* charP '"')

bName :: Parser BName
bName = fmap Name $ fmap (:) (predicateP isAlpha "Expected a alphabet.") <*> (spanP isAlphaNum) 

a = stringP "bar"
b = stringP "hello"
c = a <|> b 

startParser parser input = runParser parser (0, 0, input)
