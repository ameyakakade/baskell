module BParser where

import Control.Applicative
import Control.Monad.State.Lazy
import Data.Char
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as E

import Parser

type BProgram = [BDefinition]

data BDefinition = FDefinition { fName :: BName, fArgs :: [BName], fStatement :: BStatement }
                 | GlobalVar { vName :: BName, vSize :: Maybe Int, vInit :: [BIVal] }
                 | NakedFunction { nfName :: BName, nfAsm :: [String] }
                 | VariadicFunction { vfName :: BName, vfMinArgs :: Int }
                 deriving (Eq, Show)

data BIVal = IConstant BConstant
           | IName BName
           deriving (Eq, Show)

data BStatement = Auto      [(BName, Maybe Int)]
                | Extrn     [BName]
                | BLabel    BName BStatement
                | Case      Int BConstant BStatement
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
             | QuestionMark
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

comment :: Parser ()
comment = do
  string "//"
  Parser.many (sat (/= '\n') "Expected newline.")
  return ()

mlcomment :: Parser ()
mlcomment = do
    string "/*"
    Parser.many (sat (/='*') "Unexpected '*'" <|> (char '*' *> sat (/= '/') "Unexpected '/'"))
    string "*/"
    return ()

junk :: Parser ()
junk = do
  Parser.many (spaces <|> comment <|> mlcomment)
  return ()

parse :: Parser a -> Parser a
parse p = do
  junk
  p

token :: Parser a -> Parser a
token p = do
  t <- p
  junk
  return t

tChar = token . char

bProgram = undefined

startParser = undefined

bName :: Parser BName
bName = try $ token $ do
    s  <- get
    fc <- sat (\x -> x == '_' || isAlpha x) "Expected '_' or an alphabet."
    rs <- Parser.many (sat isAlphaNum "Expected alphanumberic character.")
    let name = fc:rs
    let f = find (== name) keywords
    if isJust f
      then raiseError (Left (FancyError (st_loc s) (E.singleton (name ++ " is a reserved keyword."))))
      else return $ BName name (st_loc s)

keywords = ["auto", "extrn", "goto", "if", "else", "return", "switch", "case", "__asm__", "while"]

parseKeyword :: String -> Parser String
parseKeyword = token . try . string

parseInt :: Parser Int
parseInt = read <$> many1 (sat isNumber "Expected a number.")

bIVal :: Parser BIVal
bIVal = fmap IConstant bConstant
        <|> fmap IName bName

bAssign :: Parser BAssign
bAssign = fmap BinaryAssign (tChar '=' *> bBinary)
          <|> fmap BinaryAssign (bBinary <* tChar '=')
          <|> fmap (const Assign) (tChar '=')

bIncDec :: Parser BIncDec
bIncDec = fmap (const Increment) (string "++")
          <|> fmap (const Decrement) (string "--")

bUnary :: Parser BUnary
bUnary = fmap (const Negative) (tChar '-')
          <|> fmap (const Not) (tChar '!')

bBinary :: Parser BBinary
bBinary = fmap (const Or) (string "|")
          <|> fmap (const And) (string "&")
          <|> fmap (const Equal) (string "==")
          <|> fmap (const NotEqual) (string "!=")
          <|> fmap (const ShiftLeft) (string "<<")
          <|> fmap (const ShiftRight) (string ">>")
          <|> fmap (const LessThanOrEqual) (string "<=")
          <|> fmap (const LessThan) (string "<")
          <|> fmap (const MoreThanOrEqual) (string ">=")
          <|> fmap (const MoreThan) (string ">")
          <|> fmap (const Add) (string "+")
          <|> fmap (const Subtract) (string "-")
          <|> fmap (const Modulo) (string "%")
          <|> fmap (const Multiply) (string "*")
          <|> fmap (const Divide) (string "/")
          <|> fmap (const QuestionMark) (string "?")

bConstant = fmap Digit parseInt

-- Rewrite the pratt parser to handle all the unary, binary, ternary
-- operations on lvalue and rvalues. Parse lvalue rvalue in the
-- pratter itself and decide what to based on that

-- We should only need `singleLValue` and `singleRValue` parsers.

parseExpr :: Int -> Parser BRValue
parseExpr minBP = token (bSingleRValue <|> fmap RLValue bSingleLValue) >>= loop
  where loop lhs = (
            do
                op <- token bBinary
                case op of
                  QuestionMark -> if minBP == 0
                                  then (do
                                             t <- parseExpr 0 <* tChar ':'
                                             f <- parseExpr 0
                                             return (Ternary lhs t f)
                                             get >>= \s -> raiseError $ Left (FancyError (st_loc s) (E.singleton "hi"))
                                             )
                                  else return lhs
                  a -> do
                      let (lbp, rbp) = bindingPower op
                      if lbp<minBP
                        then return lhs
                        else do
                          rhs <- parseExpr rbp
                          flhs <- loop (Binary lhs op rhs)
                          return flhs
            ) <|> (
            do
                assign <- bAssign
                case lhs of
                  RLValue a -> undefined
                  otherwise -> get >>= \s -> raiseError $ Left (FancyError (st_loc s) (E.singleton "Need L value to use `=` operator"))
            ) <|> return lhs
-- TODO: Fix alternative instance because the error at line 230 should
-- be shown. It is shown if we remove the assign do block.

bRValue = parseExpr 0

bSingleRValue :: Parser BRValue
bSingleRValue = IncDecPost <$> bLValue <*> bIncDec
                <|> IncDecPre <$> bIncDec <*> bLValue
                <|> RUnary <$> bUnary <*> bSingleRValue
                <|> GetAddress <$> (tChar '&' *> bLValue)
                <|> bRValueOnly >>= (\rv ->
                                        (do
                                              tChar '('
                                              args <- sepBy bRValue (tChar ',')
                                              tChar ')'
                                              return $ FunctionCall rv args
                                        ) <|> return rv)

bLValue = bSingleLValue >>=
          (\lv ->
              (do
                    rvs <- many1 (tChar '[' *> bRValue <* tChar ']')
                    return $ let (rv:rvs') = rvs
                             in foldl' (\(Array ptr offset) newOffset ->
                                           Array (RLValue $ Array ptr offset) newOffset) (Array (RLValue lv) rv) rvs'
              ) <|> return lv)

-- TODO: Verify if fn()[] is valid B. Because array works only on (rvs) and constants.
--       Improve how arrays are parsed and not rely on fold. Maybe make a special fold-like fn

bSingleLValue :: Parser BLValue
bSingleLValue = fmap Dereference (tChar '*' *> bSingleRValue)
                <|> fmap LName bName

bRValueOnly :: Parser BRValue
bRValueOnly = fmap RConstant bConstant
              <|> (do
        junk
        tChar '('
        rv <- bRValue
        tChar ')'
        return $ BracketRValue rv
    )

bStatement :: Parser BStatement
bStatement = parse (
    ( do
          tChar '{'
          sts <- Parser.many1 bStatement
          tChar '}'
          return $ Block sts
    )
    <|> fmap Extrn (parseKeyword "extrn" *> sepBy1 bName (token $ char ','))
    <|> fmap Auto  (parseKeyword "auto" *>
                     sepBy1 ((,) <$> token bName <*> optional parseInt)
                     (tChar ',') <* tChar ';')
    <|> (do
              parseKeyword "return"
              rv <- optional bRValue
              tChar ';'
              return $ BReturn rv
        )
    <|> fmap Goto (parseKeyword "goto" *> token bRValue <* tChar ';')
    <|> fmap While (parseKeyword "while" *> tChar '(' *> token bRValue <* tChar ')') <*> bStatement
    <|> (do
              parseKeyword "if"
              tChar '('
              rv <- token bRValue
              tChar ')'
              fst <- bStatement
              snd <- optional (parseKeyword "else" *> bStatement)
              return $ IfElse rv fst snd
        )
    <|> fmap Switch (parseKeyword "switch" *> bRValue) <*> bStatement
    <|> (do
              state <- get
              parseKeyword "case"
              c <- token bConstant
              tChar ':'
              s <- bStatement
              return $ Case (st_loc state) c s
        )
    <|> try (BLabel <$> bName <* tChar ':' <*> bStatement) -- TODO: Fix the error
    <|> return Empty <* tChar ';'
    )

bDefinition :: Parser BDefinition
bDefinition = (
    do
        name <- try $ token bName
        tChar '('
        args <- sepBy (token bName) (tChar ',')
        tChar ')'
        s <- bStatement
        return $ FDefinition name args s
    ) <|> (
    do
        parseKeyword "__variadic__"
        tChar '('
        name <- try $ token bName
        tChar ','
        num <- parseInt
        tChar ')'
        return $ VariadicFunction name num
    )

-- TODO: Naked functions and global variables are not parsed

{-
bDefinition :: Parser BDefinition
bDefinition = FDefinition <$> (bName <* bws) <*>
              finiteSelectBracketed '(' ')'
               (bws *> repeatedParser (spanP (==',') *> bws *> bName <* bws) <* bws) <*> (bwsnn *> (bNakedStatements <|> bStatement))

               <|> NakedFunction <$> (bName <* bws) <*> parseInlineAsm

               <|> VariadicFunction <$> (stringP "__variadic__" *> charP '(' *> bws *> bName <* bws) <*>
               (charP ',' *> bws *> fmap fromJust parseNum <* bws <* charP ')' <* bws <* charP ';')

               <|> fmap GlobalVar (bws *> bName <* bws) <*>                                                                    -- parsing the name
               ((charP '[' *> bws *>
                 ((\x -> if isNothing x then Just 0 else x) <$> parseNum) <* bws <* charP ']')
                 <|> bws $> Nothing)
               <* bws <*>
               ((:) <$> bIVal <* bws <*> tryingRepeatedParser (charP ',' *> bws *> bIVal)
                <|> return [])
               <* charP ';'-- parsing ivals
-}
