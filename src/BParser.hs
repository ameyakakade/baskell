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
                   Modulo          -> (4, 5)
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
             | Array       BRValue BRValue
             deriving (Eq, Show)

data BConstant = Digit Int
               | Char Char
               | Chars String
               deriving (Eq, Show)

data BName = BName { name :: String, nameLoc :: Int }
           deriving (Eq, Show)

bWhiteSpace skipNewlines = wsf *> stringP "/*" *> findEnd *> wsf
      <|> wsf
      where findEnd = Parser $ \input -> do
                                (a, restIn) <- runParser (spanP (/= '*')) input
                                let a = runParser (charP '*' *> charP '/') restIn
                                if isLeft a
                                then do
                                  (_, restIn') <- runParser (charP '*') restIn
                                  runParser findEnd restIn'
                                else do
                                  let Right (_, restIn'') = a in Right ("", restIn'')
            wsf = if skipNewlines then ws else wsnn

bws = bWhiteSpace True
bwsnn = bWhiteSpace False

finiteSelectBracketed sI eI parser = fmap init (selectBracketed sI eI 0) >>> (charP sI *> parser)

pratter :: Int -> Parser BRValue
pratter minBP = bws *> bSingleRValue >>= loop
    where loop lhs = Parser
                     $ \(c,i) ->
                         if null i
                         then Right (lhs, (c,i))
                         else do
                           let input = (c,i)
                           (op, restIn) <- runParser (bws *> bBinary) input
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
bBinary = fmap (const Or) (stringP "|")
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
bRValue = (ignoreErrorIndex ((,,) <$> spanP (/='?') <* charP '?' <*> spanP (/=':') <* charP ':' <*> spanP (const True)) >>=
           \(c,l,r) -> Parser $ \input -> do
                       (ce, _) <- startParser bRValue c
                       (le, _) <- startParser bRValue l
                       (re, _) <- startParser bRValue r
                       return (Ternary ce le re, input)
           )
           <|> newErr "Could not parse expression." (pratter 0)
           <|> Assignment <$> (bLValue <* ws) <*> (bAssign <* ws) <*> bRValue
           <|> FunctionCall <$> bSingleRValue <*> 
                  finiteSelectBracketed '(' ')' (ws *> repeatedParser (spanP (==',') *> ws *> (spanP (/=',') >>> bRValue) <* ws) <* ws)

bLValue :: Parser BLValue
bLValue = fmap Array bRValueSingleLValue <*> finiteSelectBracketed '[' ']' bRValue
          <|> bSingleLValue

bSingleRValue :: Parser BRValue
bSingleRValue = fmap RLValue bLValue
                <|> bRValueOnly

bSingleLValue :: Parser BLValue
bSingleLValue = fmap Dereference (charP '*' *> bRValue)
                <|> fmap LName bName

bRValueSingleLValue :: Parser BRValue
bRValueSingleLValue = fmap RLValue bSingleLValue
                      <|> bRValueOnly

bRValueOnly :: Parser BRValue
bRValueOnly = fmap RConstant bConstant
              <|> fmap BracketRValue (ws *> finiteSelectBracketed '(' ')' bRValue <* ws)

bStatement :: Parser BStatement
bStatement = fmap Block (bws *> finiteSelectBracketed '{' '}' (repeatedParser (bws *> bStatement <* bws)))
                  <|> fmap While (stringP "while" *> bws *> (selectBracketed '(' ')' 0 >>> bRValue)) <*> bStatement
                  <|> fmap Goto (keywordParser "goto" *>
                                 newErr "Expected a RValue" (selSt >>> bRValue))
                  <|> fmap Extrn (keywordParser "extrn" *>
                                 newErr "Expected a name." (selSt >>> ((:) <$> (bName <* bws) <*> repeatedParser (bws *> charP ',' *> bws *> bName)) ))
                  <|> fmap Auto (keywordParser "auto" *>
                                 newErr "Expected a name." (selSt >>> (let f = (,) <$> (bName <* bws) <*> parseNum
                                                                                                     in (:) <$> (f <* bws) <*> repeatedParser (charP ',' *> f)) ))
                  <|> fmap IfElse (stringP "if" *> bws *> (selectBracketed '(' ')' 0 >>> bRValue) <* bws) <*>
                      bStatement <*> (Just <$> (bws *> stringP "else" *> bws *> bStatement))
                  <|> fmap IfElse (stringP "if" *> bws *> (selectBracketed '(' ')' 0 >>> bRValue) <* bws) <*>
                      bStatement <*> return Nothing
                  <|> fmap BReturn (stringP "return" *> bws *> charP ';' *> pure Nothing)
                  <|> fmap BReturn (keywordParser "return" *> (selSt >>> fmap Just bRValue))
                  <|> fmap SRValue (selSt >>> ignoreErrorIndex bRValue)

    where selSt = spanP (\x -> all ($ x) [(/=';'), (/='\n')]) <* charP ';'
          keywordParser keyword = stringP keyword <* keywordSpacer keyword <* bws
          keywordSpacer i = Parser $ \input -> do
                            runParser (predicateP (\x -> isSpace x || (x=='/')) ("Expected " ++ i ++ ".")) input
                            return ("", input)

parseNum :: Parser (Maybe Int)
parseNum = (\s -> if null s then Nothing else Just (read s)) <$> spanP isNumber

bDefinition :: Parser BDefinition
bDefinition = FDefinition <$> (bName <* bws) <*>
              finiteSelectBracketed '(' ')'
               (bws *> repeatedParser (spanP (==',') *> bws *> bName <* bws) <* bws) <*> (bwsnn *> bStatement)

bProgram :: Parser BProgram
bProgram = repeatedParser (bws *> bDefinition <* bws)
