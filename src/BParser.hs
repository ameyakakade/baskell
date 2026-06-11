module BParser where

import Parser

import Data.Char
import Data.Either
import Data.Maybe
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
                   Add             -> (3, 4)
                   Subtract        -> (3, 4)
                   Multiply        -> (5, 6)
                   Divide          -> (5, 6)
                   Modulo          -> (5, 6)
                   Equal           -> (1, 2)
                   NotEqual        -> (1, 2)
                   Or              -> (0, 1)
                   And             -> (0, 1)
                   LessThanOrEqual -> (1, 2)
                   LessThan        -> (1, 2)
                   MoreThanOrEqual -> (1, 2)
                   MoreThan        -> (1, 2)
                   ShiftLeft       -> (1, 2)
                   ShiftRight      -> (1, 2)

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

escapedStringP :: (Char -> Bool) -> Parser String
escapedStringP predicate = Parser f
  where
    f (c, []) = Right ([], (c, []))
    f (c, x:xs)
      | x=='*' = let (a:as) = xs
                     a' = escapedChars a
                 in if isNothing a'
                    then Left (["Invalid escape char"] ,(c, xs))
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
                       '0' -> Just '\0'
                       'n' -> Just '\n'
                       '"' -> Just '\"'
                       '*' -> Just '*'
                       a   -> Nothing

safeSpanP :: (Char -> Bool) -> Parser String
safeSpanP p = Parser $ \(c,i) -> if i/=[]
                                             then
                                                 let b = runParser (sp <|> fmap (:[]) (predicateP p "Error in safeSpanP")) (c,i)
                                                 in if isRight b
                                                 then do
                                                   let Right (ob, restIn) = b
                                                   (bs, restIn') <- runParser (safeSpanP p) restIn
                                                   return (ob++bs, restIn')
                                                 else return ([], (c,i))
                                      else return ([], (c,i))
    where sp = (\x y z -> [x]++y++[z]) <$> charP '"' <*> escapedStringP (/='"') <*> charP '"'

-- this function selects a string surrounded by brackets.
-- it even works for nested brackets

selectBracketed sI eI n = Parser $ \input -> do
                            let o = runParser (newErr ("Expected " ++ "'" ++ [sI] ++ "' " ++ "'" ++ [eI] ++ "' pair, got mismatched brackets" ) $ selectBracketedE sI eI n) input
                            if isLeft o then (\(Left (err, (loc, s))) -> Left (err, (fst input, s))) o else o

selectBracketedE :: Char -> Char -> Int -> Parser String
selectBracketedE sI eI n = (charP eI <|> charP sI) >>= f
    where p c = c /= sI && c /= eI
          f b = Parser $ \i ->
                let z bs ns = do
                      (s, restIn) <- runParser (safeSpanP p) i
                      (a, restIn') <- runParser (selectBracketedE sI eI ns) restIn
                      Right (bs++s++a, restIn')
                in
                  if b==eI
                  then if n == 1
                       then Right ([b], i)
                       else z [b] (n-1)
                  else z [b] (n+1)

finiteSelectBracketed sI eI parser = fmap init (selectBracketed sI eI 0) >>> (charP sI *> parser)

pratter :: Int -> Parser BRValue
pratter minBP = bws *> bSingleRValue <* bws >>= loop
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
            <|> fmap Char (charP '`' *> Parser (\input -> do
                                           (a, restIn) <- runParser (escapedStringP (/='`')) input
                                           if length a == 1
                                           then let [a1]=a in return (a1, restIn)
                                           else Left (["Expected only one character"], restIn)
                                        ) <* charP '`')
            <|> fmap Chars (charP '"' *> escapedStringP (/='"') <* charP '"')

bName :: Parser BName
bName = Parser $ \(loc, i) -> do
          (r, restIn) <- runParser (fmap (:) (predicateP isAlpha "Expected a alphabet.") <*> spanP isAlphaNum) (loc, i)
          return (BName r loc, restIn)

bRValue :: Parser BRValue
bRValue = (ignoreErrorIndex ((,,) <$> spanP (/='?') <* charP '?' <*> spanP (/=':') <* charP ':' <*> spanP (const True)) >>=
           \(c,l,r) -> Parser $ \input -> do
                       (ce, _) <- startParser (bws *> bRValue) c
                       (le, _) <- startParser (bws *> bRValue) l
                       (re, _) <- startParser (bws *> bRValue) r
                       return (Ternary ce le re, input)
           )
           <|> newErr "Could not parse expression." (pratter 0)
           <|> Assignment <$> (bLValue <* ws) <*> (bAssign <* ws) <*> bRValue
           <|> FunctionCall <$> bSingleRValue <*> 
                  finiteSelectBracketed '(' ')' (ws *> repeatedParser (spanP (==',') *> ws *> (safeSpanP (/=',') >>> bRValue) <* ws))

bLValue :: Parser BLValue
bLValue = fmap Array bRValueSingleLValue <*> finiteSelectBracketed '[' ']' bRValue
          <|> bSingleLValue

bSingleRValue :: Parser BRValue
bSingleRValue = RUnary <$> bUnary <*> bSingleRValueNoUnary
      <|> IncDecPost <$> bSingleLValue <*> bIncDec
      <|> IncDecPre <$> bIncDec <*> bSingleLValue
      <|> bSingleRValueNoUnary

bSingleRValueNoUnary :: Parser BRValue
bSingleRValueNoUnary = fmap RLValue bLValue
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

    where selSt = safeSpanP (\x -> all ($ x) [(/=';'), (/='\n')]) <* charP ';'
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
