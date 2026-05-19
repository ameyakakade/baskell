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
  (Left (e1, (l1, c1, s1))) <|> (Left (e2, (l2, c2, s2))) = if (l1+c1) > (l2+c2) -- return the error which parsed the most
                                                            then Left (e1, (l1, c1, s1))
                                                            else Left (e2, (l2, c2, s2))
  
newErr :: String -> Parser String -> Parser String
newErr newError (Parser oldP) = Parser $ \input -> replace $ oldP input
  where replace (Right a) = (Right a)
        replace (Left (oldErr, a)) = (Left (newError:oldErr, a))

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (line, col, (y:ys))
      | y == x = Right (x, (lineN, colN, ys))
      | otherwise = Left (("Expected " ++ (show x)):[], (lineN, colN, y:ys))
      where
        colN = if isNewLine then 0 else (col+1)
        lineN = (if isNewLine then 1 else 0) + line
        isNewLine = y == '\n'
    f (line, col, []) = Left (("Expected " ++ (show x) ++ ", reached end of file."):[], (line, col, []))

stringP :: String -> Parser String
stringP input = newErr ("Expected " ++ input) $ sequenceA $ map charP input
