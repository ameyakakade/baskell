module Parser where

import Data.Char
import Control.Applicative

--           line  col  input
type Input = (Int, Int, String)

newtype BProgram = Program [BDefinition]
              deriving (Eq, Show)

data BDefinition = BDefinition
                 deriving (Eq, Show)

data BIVal = IConstant BConstant
           | IName BName
           deriving (Eq, Show)

data BStatement = Auto     [(BName, Maybe BConstant)]
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
             | Assignment    (BLValue, BAssign, BRValue)
             | IncDecPre     (BIncDec, BLValue)
             | IncDecPost    (BLValue, BIncDec)
             | RUnary        (BUnary, BRValue)
             | GetAddress    BLValue
             | Binary        (BRValue, BBinary, BRValue)
             | Ternary       (BRValue, BRValue, BRValue)
             -- TODO
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

newtype BName = Name String
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
bName = fmap Name $ fmap (:) (predicateP isAlpha "Expected a alphabet.") <*> spanP isAlphaNum

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

repeatedParser :: Parser a -> Parser [a]
repeatedParser parser = Parser $ \(l,r,i) -> if i/=[]
                                             then do
                                               (b, restIn) <- runParser parser (l,r,i)
                                               (bs, restIn') <- runParser (repeatedParser parser) restIn
                                               return (b:bs, restIn')
                                             else return ([], (l,r,i))

pratter :: Int -> Parser BRValue
pratter minBP = bSingleRValue >>= loop
    where loop lhs = Parser
                     $ \(l,c,i) ->
                         if null i
                         then Right (lhs, (l,c,i))
                         else do
                           let input = (l,c,i)
                           (op, restIn) <- runParser bBinary input
                           let (lbp, rbp) = bindingPower op
                           if lbp<minBP
                           then Right (lhs, input)
                           else do
                             (rhs, restIn') <- runParser (pratter rbp) restIn
                             (flhs, restIn'') <- runParser (loop (Binary (lhs, op, rhs))) restIn'
                             Right (flhs, restIn'')

bRValue :: Parser BRValue
bRValue = pratter 0
          <|> fmap Assignment ((,,) <$> (bLValue <* ws) <*> (bAssign <* ws) <*> bRValue)

bLValue :: Parser BLValue
bLValue = fmap LName bName
          <|> fmap Dereference (charP '*' *> bRValue)

bSingleRValue :: Parser BRValue
bSingleRValue = fmap RLValue bLValue
                <|> fmap RConstant bConstant
                <|> fmap BracketRValue (ws *> (selectBracketed '(' ')' 0 >>> bRValue) <* ws)

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

bStatement :: Parser BStatement
bStatement = newErr "Expected a statement." $ fmap SRValue ((spanP (/=';') <* charP ';') >>> bRValue)
                  <|> fmap While ((,) <$> (stringP "while" *> ws *> (selectBracketed '(' ')' 0 >>> bRValue)) <*> bStatement)
                  <|> fmap Goto (stringP "goto" *> predicateP isSpace "Expected goto." *> ws *>
                                 newErr "Expected a RValue" ((spanP (/=';') <* charP ';') >>> bRValue))
                  <|> fmap Block (selectBracketed '{' '}' 0 >>> repeatedParser (ws *> bStatement))
                  <|> fmap Extrn (stringP "extrn" *> predicateP isSpace "Expected extrn." *> ws *>
                                 newErr "Expected a name." ((spanP (/=';') <* charP ';') >>> ((:) <$> (bName <* ws) <*> repeatedParser (charP ',' *> bName)) ))
                  <|> fmap Auto (stringP "auto" *> predicateP isSpace "Expected extrn." *> ws *>
                                 newErr "Expected a name." ((spanP (/=';') <* charP ';') >>> (let f = (,) <$> (bName <* ws) <*> fmap (\x -> if null x then Nothing else Just (x!!0))
                                                                                                      (repeatedParser bConstant)
                                                                                              in (:) <$> (f <* ws) <*> repeatedParser (charP ',' *> f)) ))

visualizeTree :: Int -> BRValue -> String
visualizeTree d (RConstant a) = show a
visualizeTree d (Binary (l,o,r)) = i ++ so ++ "\n" ++ i ++ i ++ lo ++ "\n" ++ i ++ i ++ ro
    where so = show o
          lo = visualizeTree (d+1) l
          ro = visualizeTree (d+1) r
          i = replicate (d*2) ' '

a = pratter 0
e = stringP "atleast" <* ws
b = spanP (/=';') <* charP ';' <* ws
c = b >>> a
f = b >>> e
d = (,,) <$> f <*> f <*> c

test i = putStr $ (++"\n") $ visualizeTree 0 a
         where r = startParser (pratter 0) i
               (Right (a,_)) = r
