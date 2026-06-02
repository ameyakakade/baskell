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
           deriving (Eq, Show)


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
             deriving (Eq, Show)

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
          deriving (Eq, Show)

data ScopeEvent = Declare {declName :: String, declIndex :: Int}
                | BlockBegin Int -- this is the block number
                | BlockEnd Int
                  deriving (Eq, Show)

data Storage = Storage {
      storageExternal :: String,
      storageAuto :: Int
    } deriving (Eq, Show)

data Var = Var {
      varName :: String,
      varStorage :: Storage,
      varLoc :: Int
    } deriving (Eq, Show)

data OpWithLocation = OpWithLocation {
      opCode :: Op,
      scopeEventsCount :: Int
    } deriving (Eq, Show)

data Function = Function {
      funName :: String,
      funLoc :: Int,
      body :: [OpWithLocation],
      paramsCount :: Int,
      autoVarCount :: Int,
      scopeEvents :: [ScopeEvent]
    } deriving (Eq, Show)

data IRProgram = IRProgram {
      functions :: [Function],
      staticData :: [Word8],
      extrns :: [String]
    } deriving (Eq, Show)

data Compiler = Compiler {
      program :: IRProgram,
      errors :: [String],

      vars :: [[Var]],

      functionNames :: [BName],

      functionBody :: [OpWithLocation],
      functionScopeEvents :: [ScopeEvent],
      functionBlocksCount :: Int,

      cAutoVarCount :: Int,
      cAutoVarCountMax :: Int
    } deriving (Eq, Show)

emptyCompiler = Compiler (IRProgram [] [] []) [] [] [] [] [] 0 0 0

initCompiler :: BProgram -> Compiler
initCompiler a = emptyCompiler { functionNames = (map fName a) }

allocateAutoVariable :: Compiler -> Compiler
allocateAutoVariable c = c { cAutoVarCount = count, cAutoVarCountMax = max (cAutoVarCountMax c) count }
    where count = cAutoVarCount c + 1

deallocateAutoVariable :: Compiler -> Compiler
deallocateAutoVariable c = c { cAutoVarCount = count}
    where count = cAutoVarCount c - 1

declareVar
              
gProgram :: BProgram -> Compiler
gProgram a = foldr gDefinition (initCompiler a) a

gDefinition :: BDefinition -> Compiler -> Compiler
gDefinition (FDefinition name args block) = gFunction name args block

gFunction :: BName -> [BName] -> BStatement -> Compiler -> Compiler
gFunction bname args block c = emptyCompiler { program = newProgram, errors = (errors nc), functionNames = (functionNames nc) }
    where nc = gStatement c block
          newFunc = Function (name bname)
                    (nameLoc bname)
                    (functionBody c)
                    (length args)
                    (cAutoVarCountMax c)
                    (functionScopeEvents c)
          newProgram = let irp = program c in irp { functions = newFunc:functions irp }

gStatement :: Compiler -> BStatement -> Compiler
gStatement c statement = case statement of
                               Block   a -> gBlock c a
                               Extrn   a -> undefined
                               SRValue a -> gRValue c a

gBlock :: Compiler -> [BStatement] -> Compiler
gBlock ss = undefined

gRValue :: Compiler -> BRValue -> Compiler
gRValue fs rvalue = case rvalue of
                      FunctionCall f args -> undefined

tee = [FDefinition (BName {name = "main", nameLoc = 0}) []
       (Block [Extrn [BName {name = "hi", nameLoc = 18}]
              ,SRValue (FunctionCall (RLValue (LName (BName {name = "hi", nameLoc = 26}))) [])])]

-- teeparsed = IRProgram [BName {name = "main", nameLoc = 0}] f sd ex
--     where f = [(Function "main" bo 0 0 0)]
--           bo = [Funcall 0 (External "hi") []]
--           sd = []
--           ex = ["hi"]
