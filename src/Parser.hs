module Parser where
import Data.Char
import Data.Either
import Data.Tuple
import Data.Set (Set)
import qualified Data.Set as E
import Data.List.NonEmpty
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Lazy

data ErrorItem
  = -- Non-empty stream of tokens
    Token String
  | -- Label (cannot be empty)
    Label (NonEmpty Char)
  | -- End of input
    EndOfInput
    deriving (Show, Eq, Ord)

data ParserError
  = -- Offset : Unexpected tokens if any : Set of expected tokens
    TrivialError Int (Maybe ErrorItem) (Set ErrorItem)
  | -- Offset : Set of custom errors
    FancyError Int (Set String)
    deriving (Show, Eq)

-- FancyErrors are favoured over TrivialErrors.
-- When merging two FancyErrors the strings are added.
-- When merging two TrivialErrors the exp tokens are merged.

instance Semigroup ParserError where
    (<>) (TrivialError pos1 uxp_tok1 exp_toks1) (TrivialError pos2 uxp_tok2 exp_toks2) = assert pos1 pos2
      (TrivialError pos1 uxp_tok1 (E.union exp_toks1 exp_toks2))
    (<>) (TrivialError pos1 uxp_tok1 exp_toks1) (FancyError pos2 ferr_msg)             = assert pos1 pos2
      (FancyError pos2 ferr_msg)
    (<>) (FancyError pos1 ferr_msg)             (TrivialError pos2 uxp_tok2 exp_toks2) = assert pos1 pos2
      (FancyError pos1 ferr_msg)
    (<>) (FancyError pos1 ferr_msg1)            (FancyError pos2 ferr_msg2)            = assert pos1 pos2
      (FancyError pos1 (E.union ferr_msg1 ferr_msg2))

-- Errors being merged should never have different position.
assert :: Int -> Int -> ParserError -> ParserError
assert p1 p2 exp = if p1 == p2 then exp else undefined 

data ParserState = ParserState
    { st_str :: String,
      st_loc :: Int
    } deriving (Show)

-- We check if the position is equal instead of checking if the
-- remaining output is same.
instance Eq ParserState where
    s1 == s2 = (st_loc s1) == (st_loc s2)

-- The stateT either returns a tuple with our state and value or
-- ParserError. This means we don't get a state when we get a error,
-- just error location. This is why we don't use stateT

-- Our parser should keep the state seperate from result and make it
-- possible to modify the result without touching the state, for
-- example raising an error. This is not possible when using stateT
-- monad as it keeps the result and state in a (,) which is hard to
-- manage. We will define our custom `state monad`.
newtype Parser a = Parser { unwrapParser :: ParserState -> (ParserState, Either ParserError a) }

runParser :: Parser a -> String -> (ParserState, Either ParserError a)
runParser p s = unwrapParser p (ParserState s 0)

instance Functor Parser where
    fmap f (Parser a) = Parser $ \s -> fmap (fmap f) (a s)
    -- fmap over tuple and either

instance Applicative Parser where
    pure a = Parser $ \s -> (s, pure a)
    (<*>) f p = Parser $ \s -> (\(ns, atb) -> let (nns, res) = unwrapParser p ns
                                              in (nns, atb <*> res))
                               (unwrapParser f s)

instance Monad Parser where
    (>>=) p f = Parser $ \s -> let (ns, res) = unwrapParser p s
                               in case res of
                                    Right a -> unwrapParser (f a) ns
                                    Left  a -> (ns, Left a)
-- If its Right then use the bind or else just return the error.

-- Be careful when designing this
instance Alternative Parser where
    empty = Parser $ \s -> (s, Left (TrivialError 0 Nothing (E.singleton EndOfInput)))
    (<|>) (Parser p1) (Parser p2) = Parser $
      \s -> let (s1, fstChoice) = p1 s
            in if isRight fstChoice                            -- If first parser works, return
               then (s1, fstChoice)
               else if s == s1 -- If failed, check if we can backtrack
                    then let (s2, sndChoice) = p2 s            -- If yes, try second parser.
                         in if isRight sndChoice               -- Second parser works, return
                            then (s2, sndChoice)
                            else if s == s2 -- If second parser fails, check if we can merge
                                 then let (Left a) = fstChoice -- Merging errors
                                          (Left b) = sndChoice
                                      in (s, Left (a <> b))
                                 else (s, sndChoice)           -- Not merging, returning second parser's failure
                    else (s, fstChoice)

-- if the error and saved position is not equal, do not backtrack and
-- fail immediately. if however, the saved position and position of
-- error is the same, that means the parser was atomic and we
-- backtrack and move on to the next parser. make sure to keep this
-- implementation efficient and not cause space leaks

-- Errors have a seperate type from the result, this makes it a little
-- tricky to define things like >>=. For alternative, we can merge
-- errors easily as they will be of the same type. Might need to
-- rethink how the errors are handled in <*>

-- Error type is going to be `String` most of the type as most
-- primitive tokens will be strings. Maybe we dont need to make it a
-- parameter.

instance MonadState ParserState Parser where
    state f = Parser $ \s -> Right <$> swap (f s)

satisfy :: (a -> Bool) -> String -> Parser a -> Parser a
satisfy f err_msg p = do
    s <- get
    a <- p
    if f a
      then return a
      else raiseError $ Left (FancyError (st_loc s) (E.singleton err_msg))

-- raiseError function does not replace errors from the source parser
-- as it will be inside a do block, the parser monad will propogate
-- the error itself and there is nothing we can do to stop it.

-- This function just modifies the result of the parser with the
-- position from the state. You provide the function to check and in
-- the do block you also provide the item on which to run the fn
raiseError :: Either ParserError a -> Parser a
raiseError err = if isLeft err
                 then Parser $ \s -> (s, err)
                 else undefined -- Please provide a error.

-- This raises error, i.e uses the previous parsed count. It does not however, restore the previous state.

-- Make a function to replace a error

-- Make this fail on eof
item :: Parser Char
item = Parser $ \(ParserState s count) -> case s of
                                            [] -> (ParserState s count, Left (TrivialError count Nothing (E.singleton EndOfInput)))
                                            (x:xs) -> (ParserState xs (count + 1), Right x)

try :: Parser a -> Parser a
try (Parser p) = Parser $ \s -> let (ns, r) = p s
                                in if isRight r
                                   then (ns, r)
                                   else (s, r)

char :: Char -> Parser Char
char i = do
    s <- get
    c <- item
    if c == i
      then do
        return i
      else do
        put s
        raiseError (Left (TrivialError (st_loc s) Nothing (E.singleton (Token [i]))))

-- define string without using char for better error messages
-- we cannot use `raiseError` to error out when the char parser errors
-- because that function cannot replace errors. Make a function to
-- replace errors.
string :: String -> Parser String
string str = Parser $ \s -> let (s', res) = unwrapParser (traverse char str) s
                            in if (isRight res)
                               then (s', res)
                               else (s, Left (TrivialError (st_loc s) Nothing (E.singleton (Token str))) )
                                    -- this doesn't propogate the previous errors forward but replaces them.

tls :: Parser String
tls = do
    a <- item
    b <- item
    c <- item
    return [a,b,c]

f = runParser (string "this" <|> string "damn")

-- All primitives that consume input are atomic, if they fail, they
-- backtrack automatically. Example, a string parser will match the
-- entire string, or fail without consuming any input.

-- Suppose if we get auto, we know that it will followed by a list of
-- variables.

-- Hell yea this works as expected.
alternatives :: Parser (Char, Char)
alternatives = foo <|> bar
  where
    foo = try $ (,) <$> char 'a' <*> char 'b'
    bar = try $ (,) <$> char 'a' <*> char 'c'

-- My parser successfully works with this even though it should
-- fail. This is because my parser has infinite backtracking. A proper
-- parser will check if the first character matches and then 'choose'
-- that branch, it will not check other branches if that branch
-- fails. This probably means I have to design <*> and <|> with alot
-- of care, and add a try keyword that allows for backtracking when
-- needed.

-- When a atomic parser fails, it unconsumes the input by giving us
-- the previous position. But, if there are two or more parser's
-- chained together, the position at which it will fail will not
-- always be equal to the previous position, and this is when we will
-- show the error and not care about other branches. To remedy this we
-- need `try` which allows us to backtrack a non atomic parser.

many1 :: Parser a -> Parser [a]
many1 p = do
    a <- p
    as <- Parser.many p
    return (a:as)

many :: Parser a -> Parser [a]
many p = many1 p <|> return []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = do
    a <- p
    as <- Parser.many $ try $ sep *> p
    return (a:as)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy p sep <|> return []

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest acc = (do
                     f <- op
                     x <- p 
                     rest (f acc x)
               ) <|> return acc

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = p >>= rest
  where
    rest x = (do
                   f <- op
                   xs <- chainr1 p op
                   return (f x xs)
             ) <|> return x

chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op v = chainr1 p op <|> return v

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op v = chainl1 p op <|> return v
