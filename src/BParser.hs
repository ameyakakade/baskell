module BParser where

import Parser

import Data.Char
import Control.Applicative

type BProgram = [BDefinition]

data BDefinition = FDefinition {fName :: BName, fArgs :: [BName], fStatement :: BStatement}
                 deriving (Eq, Show)

data BIVal = IConstant BConstant
           | IName BName
           deriving (Eq, Show)

data BStatement = Auto     [(BName, Maybe Int)]
                | Extrn    [BName]
                | Default  [(BConstant, BStatement)] -- TODO: Confirm if this correct
                | Case     [(BConstant, BStatement)]
                | Block    [BStatement]
                | IfElse   BRValue BStatement BStatement
                | While    BRValue BStatement
                | Switch   BRValue BStatement
                | Goto     BRValue
                | Return   BRValue
                | SRValue  BRValue
                deriving (Eq, Show)

data BRValue = BracketRValue BRValue
             | RLValue       BLValue
             | RConstant     BConstant
             | Assignment    BLValue BAssign BRValue
             | IncDecPre     BIncDec BLValue
             | IncDecPost    BLValue BIncDec
             | RUnary        BUnary BRValue
             | GetAddress    BLValue
             | Binary        BRValue BBinary BRValue
             | Ternary       BRValue BRValue BRValue
             | FunctionCall  BRValue [BRValue]
             deriving (Eq, Show)

          -- left binding power, right binding power
bindingPower :: BBinary -> (Int, Int)
bindingPower Add             = (1, 2)
bindingPower Subtract        = (1, 2)
bindingPower Multiply        = (3, 4)
bindingPower Divide          = (3, 4)

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

data BName = BName { name :: String, nameLoc :: Int }
           deriving (Eq, Show)

finiteSelectBracketed sI eI parser = fmap init (selectBracketed sI eI 0) >>> (charP sI *> parser)

pratter :: Int -> Parser BRValue
pratter minBP = bSingleRValue >>= loop
    where loop lhs = Parser
                     $ \(c,i) ->
                         if null i
                         then Right (lhs, (c,i))
                         else do
                           let input = (c,i)
                           (op, restIn) <- runParser bBinary input
                           let (lbp, rbp) = bindingPower op
                           if lbp<minBP
                           then Right (lhs, input)
                           else do
                             (rhs, restIn') <- runParser (pratter rbp) restIn
                             (flhs, restIn'') <- runParser (loop (Binary lhs op rhs)) restIn'
                             Right (flhs, restIn'')

bIVal :: Parser BIVal
bIVal = fmap IConstant bConstant
        <|> fmap IName bName

bAssign :: Parser BAssign
bAssign = fmap BinaryAssign (charP '=' *> bBinary)
          <|> fmap (const Assign) (charP '=')

bIncDec :: Parser BIncDec
bIncDec = newErr "Expected increment or decrement" $
          fmap (const Increment) (stringP "++")
          <|> fmap (const Decrement) (stringP "--")

bUnary :: Parser BUnary
bUnary = newErr "Expected a unary operator" $
          fmap (const Negative) (charP '-')
          <|> fmap (const Exclamation) (charP '!')

bBinary :: Parser BBinary
bBinary = newErr "Expected a binary operator" $
          fmap (const Or) (stringP "|")
          <|> fmap (const And) (stringP "&")
          <|> fmap (const Equal) (stringP "==")
          <|> fmap (const NotEqual) (stringP "!=")
          <|> fmap (const ShiftLeft) (stringP "<<")
          <|> fmap (const ShiftRight) (stringP ">>")
          <|> fmap (const LessThanOrEqual) (stringP "<=")
          <|> fmap (const LessThan) (stringP "<")
          <|> fmap (const MoreThanOrEqual) (stringP ">=")
          <|> fmap (const MoreThan) (stringP ">")
          <|> fmap (const Add) (stringP "+")
          <|> fmap (const Subtract) (stringP "-")
          <|> fmap (const Modulo) (stringP "%")
          <|> fmap (const Multiply) (stringP "*")
          <|> fmap (const Divide) (stringP "/")

bConstant :: Parser BConstant
bConstant = fmap (Digit . read) (fmap (:) (predicateP isDigit "Expected atleast one digit") <*> spanP isDigit)
            <|> fmap Char (charP '`' *> predicateP isAlpha "Expected a character" <* charP '`')
            <|> fmap Chars (charP '"' *> spanP (/='"') <* charP '"')

bName :: Parser BName
bName = Parser $ \(loc, i) -> do
          (r, restIn) <- runParser (fmap (:) (predicateP isAlpha "Expected a alphabet.") <*> spanP isAlphaNum) (loc, i)
          return (BName r loc, restIn)

bRValue :: Parser BRValue
bRValue = pratter 0
          <|> Assignment <$> (bLValue <* ws) <*> (bAssign <* ws) <*> bRValue
          <|> FunctionCall <$> bSingleRValue <*> 
                  finiteSelectBracketed '(' ')' (ws *> repeatedParser (spanP (==',') *> ws *> bSingleRValue <* ws) <* ws)

bLValue :: Parser BLValue
bLValue = fmap LName bName
          <|> fmap Dereference (charP '*' *> bRValue)

bSingleRValue :: Parser BRValue
bSingleRValue = fmap RLValue bLValue
                <|> fmap RConstant bConstant
                <|> fmap BracketRValue (ws *> finiteSelectBracketed '(' ')' bRValue <* ws)

bStatement :: Parser BStatement
bStatement = newErr "Expected a statement." $ fmap SRValue ((spanP (/=';') <* charP ';') >>> bRValue)
                  <|> While <$> (stringP "while" *> ws *> (selectBracketed '(' ')' 0 >>> bRValue)) <*> bStatement
                  <|> fmap Goto (stringP "goto" *> predicateP isSpace "Expected goto." *> ws *>
                                 newErr "Expected a RValue" ((spanP (/=';') <* charP ';') >>> bRValue))
                  <|> fmap Block (finiteSelectBracketed '{' '}' (repeatedParser (ws *> bStatement <* ws)))
                  <|> fmap Extrn (stringP "extrn" *> predicateP isSpace "Expected extrn." *> ws *>
                                 newErr "Expected a name." ((spanP (/=';') <* charP ';') >>> ((:) <$> (bName <* ws) <*> repeatedParser (charP ',' *> bName)) ))
                  <|> fmap Auto (stringP "auto" *> predicateP isSpace "Expected extrn." *> ws *>
                                 newErr "Expected a name." ((spanP (/=';') <* charP ';') >>> (let f = (,) <$> (bName <* ws) <*> parseNum
                                                                                                     in (:) <$> (f <* ws) <*> repeatedParser (charP ',' *> f)) ))
parseNum :: Parser (Maybe Int)
parseNum = (\s -> if null s then Nothing else Just (read s)) <$> spanP isNumber
           
bDefinition :: Parser BDefinition
bDefinition = newErr "Expected a definition"
              $ FDefinition <$> (bName <* ws) <*>
              finiteSelectBracketed '(' ')'
               (ws *> repeatedParser (spanP (==',') *> ws *> bName <* ws) <* ws) <*> bStatement

bProgram :: Parser BProgram
bProgram = repeatedParser (ws *> bDefinition <* ws)
