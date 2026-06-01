-- this will generate IR code from AST.

module Generator where

import BParser
import Parser
import Data.Word

data Arg = AutoVar     Word
         | Deref       Word
         | RefAutoVar  Word
         | RefExternal String
         | External    String
         | Literal     Word -- has to be word
         | DataOffset  Word
           deriving (Show)


data BinOp = Plus
           | Minus
           | Mult
           | Div
           | Mod
           | Equal
           | NotEqual
           | Less
           | LessEqual
           | Greater
           | GreaterEqual
           | BitOr
           | BitAnd
           | BitShl
           | BitShr
             deriving (Show)

data Op = UnaryNot        Word   Arg          -- result, arg
        | Negate          Word   Arg          -- result, arg
        | OpBin           BinOp  Word Arg Arg -- binop, index, lhs, rhs
        | Index           Word   Arg  Arg     -- result, arg, offset
        | AutoAssign      Word   Arg          -- index, arg
        | ExternalAssign  String Arg          -- name, arg
        | Store           Word   Arg          -- index, arg
        | Funcall         Word   Arg  [Arg]   -- result, fn, args
        | Label           Word                -- label
        | JmpLabel        Word                -- label
        | JmpIfNotLabel   Word   Arg          -- label
        | Return          (Maybe Arg)         -- arg
          deriving (Show)

data Function = Function {
      name :: String,
      body :: [Op],
      params_count :: Word,
      auto_var_count :: Word
    } deriving (Show)

data IRProgram = IRProgram {
      functions :: [Function],
      staticData :: [Word8],
      extrns :: [String]
    } deriving (Show)

gProgram :: BProgram -> IRProgram
gProgram = foldr gDefinition (IRProgram [] [] [])

gDefinition :: BDefinition -> IRProgram -> IRProgram
gDefinition (FDefinition name args block) = gFunction name args block

gFunction :: BName -> [BName] -> BStatement -> IRProgram -> IRProgram
gFunction name args block = undefined
