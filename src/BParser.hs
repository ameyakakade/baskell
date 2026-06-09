module BParser where

import Parser

import Data.Char
import Data.Either
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
                | IfElse   BRValue BStatement (Maybe BStatement)
                | While    BRValue BStatement
                | Switch   BRValue BStatement
                | Goto     BRValue
                | BReturn   (Maybe BRValue)
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
bindingPower b = case b of
                   Add             -> (2, 3)
                   Subtract        -> (2, 3)
                   Multiply        -> (4, 5)
                   Divide          -> (4, 5)
                   Equal           -> (0, 1)
                   NotEqual        -> (0, 1)
                   Or              -> (0, 1)
                   And             -> (0, 1)
                   LessThanOrEqual -> (0, 1)
                   LessThan        -> (0, 1)
                   MoreThanOrEqual -> (0, 1)
                   MoreThan        -> (0, 1)

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

bRValue = Parser $ \input -> do
            let o = runParser bRValueE input
            if isLeft o then (\(Left (err, (loc, s))) -> Left (err, (fst input, s))) o else o

bRValueE :: Parser BRValue
bRValueE = pratter 0
          <|> Assignment <$> (bLValue <* ws) <*> (bAssign <* ws) <*> bRValue
          <|> FunctionCall <$> bSingleRValue <*> 
                  finiteSelectBracketed '(' ')' (ws *> repeatedParser (spanP (==',') *> ws *> (spanP (/=',') >>> bRValue) <* ws) <* ws)

bLValue :: Parser BLValue
bLValue = fmap LName bName
          <|> fmap Dereference (charP '*' *> bRValue)

bSingleRValue :: Parser BRValue
bSingleRValue = fmap RLValue bLValue
                <|> fmap RConstant bConstant
                <|> fmap BracketRValue (ws *> finiteSelectBracketed '(' ')' bRValue <* ws)

bStatement :: Parser BStatement
bStatement = fmap Block (ws *> finiteSelectBracketed '{' '}' (repeatedParser (ws *> bStatement <* ws)))
                  <|> fmap While (stringP "while" *> ws *> (selectBracketed '(' ')' 0 >>> bRValue)) <*> bStatement
                  <|> fmap Goto (stringP "goto" *> predicateP isSpace "Expected goto." *> ws *>
                                 newErr "Expected a RValue" (selSt >>> bRValue))
                  <|> fmap Extrn (stringP "extrn" *> predicateP isSpace "Expected extrn." *> ws *>
                                 newErr "Expected a name." (selSt >>> ((:) <$> (bName <* ws) <*> repeatedParser (ws *> charP ',' *> ws *> bName)) ))
                  <|> fmap Auto (stringP "auto" *> predicateP isSpace "Expected auto." *> ws *>
                                 newErr "Expected a name." (selSt >>> (let f = (,) <$> (bName <* ws) <*> parseNum
                                                                                                     in (:) <$> (f <* ws) <*> repeatedParser (charP ',' *> f)) ))
                  <|> fmap IfElse (stringP "if" *> ws *> (selectBracketed '(' ')' 0 >>> bRValue)) <*>
                      bStatement <*> (Just <$> (stringP "else" *> ws *> bStatement))
                  <|> fmap IfElse (stringP "if" *> ws *> (selectBracketed '(' ')' 0 >>> bRValue)) <*>
                      bStatement <*> return Nothing
                  <|> fmap BReturn (stringP "return" *> ws *> charP ';' *> pure Nothing)
                  <|> fmap BReturn (stringP "return" *> predicateP isSpace "Expected return." *> ws *> (selSt >>> fmap Just bRValue))
                  <|> fmap SRValue (selSt >>> bRValue)

    where selSt = spanP (\x -> all ($ x) [(/=';'), (/='\n')]) <* charP ';'
                  
parseNum :: Parser (Maybe Int)
parseNum = (\s -> if null s then Nothing else Just (read s)) <$> spanP isNumber
           
bDefinition :: Parser BDefinition
bDefinition = FDefinition <$> (bName <* ws) <*>
              finiteSelectBracketed '(' ')'
               (ws *> repeatedParser (spanP (==',') *> ws *> bName <* ws) <* ws) <*> (wsnn *> bStatement)

bProgram :: Parser BProgram
bProgram = repeatedParser (ws *> bDefinition <* ws)
