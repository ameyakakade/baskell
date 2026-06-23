module BParser where

import Parser

import Data.Char
import Data.Either
import Data.Maybe
import Data.Functor
import Data.List
import Control.Applicative

type BProgram = [BDefinition]

data BDefinition = FDefinition {fName :: BName, fArgs :: [BName], fStatement :: BStatement}
                 | GlobalVar {vName :: BName, vSize :: Maybe Int, vInit :: [BIVal]}
                 | NakedFunction {nfName :: BName, nfAsm :: [String]}
                 deriving (Eq, Show)

data BIVal = IConstant BConstant
           | IName BName
           deriving (Eq, Show)

data BStatement = Auto      [(BName, Maybe Int)]
                | Extrn     [BName]
                | BLabel    BName BStatement
                | Case      BConstant BStatement
                | Block     [BStatement]
                | IfElse    BRValue BStatement (Maybe BStatement)
                | While     BRValue BStatement
                | Switch    BRValue BStatement
                | Goto      BRValue
                | BReturn   (Maybe BRValue)
                | SRValue   BRValue
                | InlineAsm [String]
                | Empty
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
            | Not
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

data BConstant = Digit       Int
               | HexConst    String
               | OctalConst  String
               | BinaryConst String
               | CharConst   Char
               | Chars       String
               deriving (Eq, Show)

data BName = BName { name :: String, nameLoc :: Int }
           deriving (Eq, Show)

keywords = ["auto", "extrn", "goto", "if", "else", "return", "switch", "case", "__asm__"]

-- WARNING: C like comments are supported
bWhiteSpace :: Bool -> Parser String
bWhiteSpace skipNewlines = wsf *> (stringP "/*" *> findEnd "*/" *> wsf
                                  <|> stringP "//" *> findEnd "\n" *> wsf
                                  <|> pure "")
    where findEnd end = Parser $ \input -> do
                          (a, restIn) <- runParser (spanP (/= head end)) input
                          let a = runParser (stringP end) restIn
                          if isLeft a
                          then do
                            (_, restIn') <- runParser (charP '*') restIn
                            runParser (findEnd end) restIn'
                          else do
                            let Right (_, restIn'') = a
                            runParser bws restIn''
          wsf = if skipNewlines then ws else wsnn

bws = bWhiteSpace True
bwsnn = bWhiteSpace False

escapedStringP :: (Char -> Bool) -> Parser String
escapedStringP predicate = Parser f
  where
    f (c, []) = Right ([], (c, []))
    f (c, x:xs)
      | x== '*' = let (a:as) = xs
                      a' = escapedChars a
                  in if isNothing a'
                     then Left (Error "Invalid escape char" (c, xs))
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

safeSpanP' :: Bool -> (Char -> Bool) -> Parser String
safeSpanP' safeBrackets p = Parser $ \(c,i) -> if i/=[]
                                              then
                                                  let b = runParser (sp <|> pp <|> fmap (:[]) (predicateP p "Error in safeSpanP")) (c,i)
                                                  in if isRight b
                                                     then do
                                                       let Right (ob, restIn) = b
                                                       (bs, restIn') <- runParser (safeSpanP' safeBrackets p) restIn
                                                       return (ob++bs, restIn')
                                                     else return ([], (c,i))
                                              else return ([], (c,i))
    where sp = (\x y z -> [x]++y++[z]) <$> charP '"' <*> escapedStringP (/='"') <*> charP '"'
          pp = if safeBrackets then selectBracketed '(' ')' 0 else empty

safeSpanP = safeSpanP' False

-- this function selects a string surrounded by brackets.
-- it even works for nested brackets

selectBracketed sI eI n = Parser $ \input -> do
                            let firstChar = runParser (charP sI) input
                            runParser ((if isLeft firstChar
                                       then replaceErr st
                                       else failureToError st) (selectBracketedE sI eI n)) input
    where st = "Expected " ++ "'" ++ [sI] ++ "' " ++ "'" ++ [eI] ++ "' pair." 

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

bIVal :: Parser BIVal
bIVal = fmap IConstant bConstant
        <|> fmap IName bName

-- WARNING: C style assignment is allowed
bAssign :: Parser BAssign
bAssign = fmap BinaryAssign (charP '=' *> bBinary)
          <|> fmap BinaryAssign (bBinary <* charP '=')
          <|> fmap (const Assign) (charP '=')

bIncDec :: Parser BIncDec
bIncDec = newErr "Expected increment or decrement" $
          fmap (const Increment) (stringP "++")
          <|> fmap (const Decrement) (stringP "--")

bUnary :: Parser BUnary
bUnary = newErr "Expected a unary operator" $
          fmap (const Negative) (charP '-')
          <|> fmap (const Not) (charP '!')

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
bConstant = parseNumConstant
            <|> fmap CharConst (charP '\'' *> Parser (\input -> do
                                           (a, restIn) <- runParser (escapedStringP (/='\'')) input
                                           if length a == 1
                                           then let [a1]=a in return (a1, restIn)
                                           -- else Left (["Expected only one character"], restIn)
                                           else return (head a, restIn)
                                        ) <* charP '\'')
            <|> fmap Chars (charP '"' *> escapedStringP (/='"') <* charP '"')

parseNumConstant :: Parser BConstant
parseNumConstant = fmap HexConst (charP '0' *> (charP 'x' <|> charP 'X') *>
                     (fmap (:) (predicateP hexChars "Expected valid hex constant.") <*> spanP hexChars))
          <|> fmap BinaryConst (charP '0' *> (charP 'b' <|> charP 'B') *>
                                (fmap (:) (predicateP binaryChars "Expected valid binary constant.") <*> spanP binaryChars))
          <|> fmap OctalConst (charP '0' *>
                               (fmap (:) (predicateP octalChars "Expected valid octal constant.") <*> spanP octalChars))
          <|> fmap (Digit . read) digitParser
    where digitParser = fmap (:) (predicateP isDigit "Expected atleast one digit") <*> spanP isDigit
          hexChars x = any ($ x) (map (==) "0123456789ABCDEFabcdef")
          binaryChars x = any ($ x) (map (==) "01")
          octalChars x = any ($ x) (map (==) "01234567")

bName :: Parser BName
bName = Parser $ \(loc, i) -> do
          (r, restIn) <- runParser (fmap (:) (predicateP (\x -> (x=='_') || isAlpha x) "Expected identifier.") <*> spanP (\x -> (x=='_') || isAlphaNum x)) (loc, i)
          let f = find (==r) keywords
          if isJust f
          then Left (Error (r ++ " is a reserved keyword.\n") (loc, i))
          else return (BName r loc, restIn)

bRValue = bRValue' True
bRValueStrict = bRValue' False

pratter :: Bool -> Int -> Parser BRValue
pratter trying minBP = bws *> (bRValueFunctionCall <|> bSingleRValue) <* bws >>= loop
    where loop lhs = Parser
                     $ \(c,i) ->
                         if null i
                         then Right (lhs, (c,i))
                         else do
                           let input = (c,i)
                           let bop = runParser (bws *> bBinary) input
                           if isLeft bop
                           then if trying
                                then return (lhs, (c,i))
                                else let Left a = bop in Left a
                           else do
                             let Right (op, restIn) = bop
                             let (lbp, rbp) = bindingPower op
                             if lbp<minBP
                             then Right (lhs, input)
                             else do
                               (rhs, restIn') <- runParser (pratter trying rbp) restIn
                               (flhs, restIn'') <- runParser (loop (Binary lhs op rhs)) restIn'
                               Right (flhs, restIn'')

bRValue' :: Bool -> Parser BRValue
bRValue' trying = (ignoreErrorIndex ((,,) <$> safeSpanP' True (/='?') <* charP '?' <*> safeSpanP' True (/=':') <* charP ':' <*> safeSpanP (const True)) >>=
                 \(c,l,r) -> Parser $ \(loc, i) -> do
                               (ce, (loc', _)) <- runParser (bws *> bRValueStrict) (loc, c)
                               (le, (loc'', _)) <- runParser (bws *> bRValueStrict) (loc', l)
                               (re, _) <- runParser (bws *> bRValueStrict) (loc'', r)
                               return (Ternary ce le re, (loc, i))
                 )
                 <|> Assignment <$> (bLValue <* ws) <*> (bAssign <* ws) <*> bRValue
                 <|> newErr "Could not parse expression." (pratter trying 0)

bLValue :: Parser BLValue
bLValue = ( do
            initRV <- bRValueSingleLValue
            tailRVs <- tryingRepeatedParser (finiteSelectBracketed '[' ']' bRValue)
            if null tailRVs then empty
            else return $ let (rv:rvs) = tailRVs in foldl' (\(Array ptr offset) newOffset -> Array (RLValue $ Array ptr offset) newOffset) (Array initRV rv) rvs
          )
          <|> bSingleLValue

bSingleRValue :: Parser BRValue
bSingleRValue = RUnary <$> bUnary <*> (bRValueFunctionCall <|> bSingleRValue)
                <|> IncDecPost <$> bLValue <*> bIncDec
                <|> IncDecPre <$> bIncDec <*> bLValue
                <|> GetAddress <$> (charP '&' *> bLValue)
                <|> bSingleRValueNoUnary

bSingleRValueNoUnary :: Parser BRValue
bSingleRValueNoUnary = fmap RLValue bLValue
                       <|> bRValueOnly

bSingleLValue :: Parser BLValue
bSingleLValue = fmap Dereference (charP '*' *> (bRValueFunctionCall <|> bSingleRValue))
                <|> fmap LName bName

bRValueSingleLValue :: Parser BRValue
bRValueSingleLValue = fmap RLValue bSingleLValue
                      <|> bRValueOnly

bRValueFunctionCall :: Parser BRValue
bRValueFunctionCall = FunctionCall <$> bSingleRValue <*>
                        finiteSelectBracketed '(' ')'
                        (bws *> Parser (\input -> do
                                    let parseArg = safeSpanP' True (/=',') >>> bRValue
                                    if null $ snd input
                                    then return ([], input)
                                    else do
                                      (p, restIn) <- runParser parseArg input
                                      let ups = runParser (repeatedParser (bws *> charP ',' *> bws *> parseArg <* bws)) restIn
                                      if isLeft ups
                                      then let Left upse = ups
                                           in Left $ case upse of
                                                       Failure oldErr i -> Error "Invalid argument to function." i
                                                       Error s i -> Error s i
                                      else let Right (ps, restIn') = ups
                                           in return (p:ps, restIn')
                                ))

bRValueOnly :: Parser BRValue
bRValueOnly = fmap RConstant bConstant
              <|> fmap BracketRValue (bws *> finiteSelectBracketed '(' ')' bRValue <* bws)

bStatement :: Parser BStatement
bStatement = fmap Block (bws *> finiteSelectBracketed '{' '}' (failureToError "Invalid statement" $ repeatedParser (bws *> bStatement <* bws)))
             <|> fmap While (stringP "while" *> bws *> (charP '(' *> bRValue <* charP ')')) <* bws <*> bStatement
             <|> fmap Goto (keywordParser "goto" *>
                            newErr "Expected a RValue" (selSt bRValue))
             <|> fmap Extrn (keywordParser "extrn" *>
                             newErr "Expected a name." (selSt ((:) <$> (bName <* bws) <*> repeatedParser (bws *> charP ',' *> bws *> bName)) ))
             <|> fmap Auto (keywordParser "auto" *>
                            newErr "Expected a name." (selSt (let f = (,) <$> (bName <* bws) <*> parseNum
                                                                  in (:) <$> (f <* bws) <*> repeatedParser (bws *> charP ',' *> bws *> f)) ))
             <|> fmap IfElse (stringP "if" *> bws *> (charP '(' *> bRValue <* charP ')') <* bws) <*>
                 bStatement <*> (Just <$> (bws *> stringP "else" *> bws *> bStatement))
             <|> fmap IfElse (stringP "if" *> bws *> (charP '(' *> bRValue <* charP ')') <* bws) <*>
                 bStatement <*> return Nothing
             <|> fmap BReturn ((stringP "return" *> bws *> charP ';') $> Nothing)
             <|> fmap BReturn (keywordParser "return" *> selSt (fmap Just bRValue))
             <|> fmap Switch (keywordParser "switch" *> bRValue) <*> bStatement
             <|> fmap Case (keywordParser "case" *> bConstant <* bws <* charP ':' <* bws) <*> bStatement
             <|> fmap InlineAsm parseInlineAsm 
             <|> ignoreErrorIndex (fmap BLabel bName <* bws <* charP ':' <* bws <*> bStatement)
             <|> Empty <$ bws <* charP ';'
             <|> fmap SRValue (selSt bRValueStrict)

selSt p = (safeSpanP (/=';') <* charP ';') >>> p
keywordParser keyword = stringP keyword <* keywordSpacer keyword <* bws
    where keywordSpacer i = Parser $ \input -> do
                              runParser (predicateP (\x -> isSpace x || (x=='/')) ("Expected " ++ i ++ ".")) input
                              return ("", input)

parseInlineAsm :: Parser [String]
parseInlineAsm = stringP "__asm__" *> bws *> (charP '(' *> bws *>
                                              (((:) <$> sc <* bws <*> tryingRepeatedParser (bws *> charP ',' *> bws *> sc))
                                              <|> ([] <$ bws)) <* bws <* charP ')' <* charP ';')
    where sc = charP '"' *> escapedStringP (/='"') <* charP '"'

parseNum :: Parser (Maybe Int)
parseNum = (\s -> if null s then Nothing else Just (read s)) <$> spanP isNumber

bDefinition :: Parser BDefinition
bDefinition = FDefinition <$> (bName <* bws) <*>
              finiteSelectBracketed '(' ')'
               (bws *> repeatedParser (spanP (==',') *> bws *> bName <* bws) <* bws) <*> (bwsnn *> (bNakedStatements <|> bStatement))
              <|> NakedFunction <$> (bName <* bws) <*> parseInlineAsm
              <|> fmap GlobalVar (bws *> bName <* bws) <*>                                                                    -- parsing the name
                      ((charP '[' *> bws *>((\x -> if isNothing x then Just 0 else x) <$> parseNum) <* bws <* charP ']') <|> bws $> Nothing) <* bws<*>     -- parsing maybe constant
                      ((:) <$> bIVal <* bws <*> tryingRepeatedParser (charP ',' *> bws *> bIVal) <|> return []) <* charP ';'-- parsing ivals

bNakedStatements :: Parser BStatement
bNakedStatements = fmap Block $ (\x y-> x ++ [y]) <$> tryingRepeatedParser (naked <* bws) <*> bStatement
    where naked = fmap Extrn (keywordParser "extrn" *>
                              newErr "Expected a name." (selSt ((:) <$> (bName <* bws) <*> repeatedParser (bws *> charP ',' *> bws *> bName)) ))
                  <|> fmap Auto (keywordParser "auto" *>
                                 newErr "Expected a name." (selSt (let f = (,) <$> (bName <* bws) <*> parseNum
                                                                   in (:) <$> (f <* bws) <*> repeatedParser (bws *> charP ',' *> bws *> f)) ))

bProgram :: Parser BProgram
bProgram = repeatedParser (bws *> bDefinition <* bws)

-- TODO: Using ** inside string literals doesnt work as rvalue;
-- TODO: Investigate if string literals are indexed properly.
-- TODO: Postpone things like escaping chars and C style assignment to generator.
--       This way we can turn those things off. Parse all you can and error out in
--       generator. It still is difficult to turn off C-like one line comments

-- TODO: Block that contains only auto or extrn without following statement should fail
