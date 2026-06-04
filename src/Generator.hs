-- this will generate IR code from AST.

module Generator where

import BParser
import Parser
import Data.Word
import Data.List
import Data.Maybe
import Data.Function

data Arg = AutoVar     Int
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
        | Index           Int   Arg  Arg      -- result, arg, offset
        | AutoAssign      Int   Arg           -- index, arg
        | ExternalAssign  String Arg          -- name, arg
        | Store           Int   Arg           -- index, arg
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

data Storage = StorageExternal String
             | StorageAuto Int
               deriving (Eq, Show)

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
initCompiler ast = emptyCompiler { functionNames = map fName ast }

allocateAutoVariable :: Int -> Compiler -> Compiler
allocateAutoVariable sizeToAlloc c = c { cAutoVarCount = count, cAutoVarCountMax = max (cAutoVarCountMax c) count }
    where count = cAutoVarCount c + sizeToAlloc

deallocateAutoVariable :: Int -> Compiler -> Compiler
deallocateAutoVariable sizeToDealloc c = c { cAutoVarCount = count}
    where count = cAutoVarCount c - sizeToDealloc

declareVarExtrn :: BName -> Compiler -> Compiler
declareVarExtrn n c = if isNothing (findVar (name n) c)
                      then c { vars = newStack:remainingScopes, program = newProgram }
                      else addError ("Redefinition of variable '" ++ name n ++ "'") c
    where newStack = newVar:uppermostScope
          newVar = Var (name n) (StorageExternal (name n)) (nameLoc n)
          remainingScopes = drop 1 (vars c)
          [uppermostScope] = take 1 (vars c)
          newProgram = (program c) { extrns = name n:extrns (program c) }

declareVarAuto :: (BName, Maybe Int) -> Compiler -> Compiler
declareVarAuto (n, size) c = if isNothing (findVar (name n) c)
                             then c' { vars = newStack:remainingScopes, functionScopeEvents = functionScopeEvents c ++ [newScopeEvent]}
                             else addError ("Redefinition of variable '" ++ name n ++ "'") c
    where newStack = newVar:uppermostScope
          newVar = Var (name n) (StorageAuto (cAutoVarCount c)) (nameLoc n)
          remainingScopes = drop 1 (vars c)
          [uppermostScope] = take 1 (vars c)
          c' = allocateAutoVariable (fromMaybe 1 size) c
          newScopeEvent = Declare (name n) (cAutoVarCount c)

addOp :: Op -> Compiler -> Compiler
addOp o c = c { functionBody = functionBody c ++ [newOp] }
    where newOp = OpWithLocation o (length (functionScopeEvents c))

addError :: String -> Compiler -> Compiler
addError s c = c { errors = errors c ++ [s] }

bogusArg = External "bogusArgument"

findVar :: String -> Compiler -> Maybe Var
findVar n c = if null foundVars then Nothing else Just (head foundVars)
    where findVar = find (\x -> varName x == n)
          foundVars = mapMaybe findVar (vars c)

gProgram :: BProgram -> Compiler
gProgram a = foldr gDefinition (initCompiler a) a

gDefinition :: BDefinition -> Compiler -> Compiler
gDefinition (FDefinition name args block) = gFunction name args block

gFunction :: BName -> [BName] -> BStatement -> Compiler -> Compiler
gFunction bname args block c = emptyCompiler { program = newestProgram, errors = errors c', functionNames = functionNames c' }
    where c' = gStatement c block

          newFunc = Function (name bname)
                    (nameLoc bname)
                    (functionBody c')
                    (length args)
                    (cAutoVarCountMax c')
                    (functionScopeEvents c')

          newestProgram = let newProgram = program c' in newProgram { functions = newFunc:functions newProgram }

gStatement :: Compiler -> BStatement -> Compiler
gStatement c statement = case statement of
                               Block   a -> gBlock c a
                               Extrn   a -> gExtrn c a
                               Auto    a -> gAuto c a
                               SRValue a -> gRValue c a

gBlock :: Compiler -> [BStatement] -> Compiler
gBlock c ss = c'' { cAutoVarCount = autoVarC }
    where c' = blockBegin c
          autoVarC = cAutoVarCount c
          c'' = c' & \x -> foldl' gStatement x ss & blockEnd (functionBlocksCount c)

blockBegin :: Compiler -> Compiler
blockBegin c = c { vars = []:vars c, functionBlocksCount = 1+functionBlocksCount c, functionScopeEvents = functionScopeEvents c++[newScopeEvent] }
    where newScopeEvent = BlockBegin (functionBlocksCount c)

blockEnd :: Int -> Compiler -> Compiler
blockEnd blockID c = c { vars = drop 1 $ vars c, functionBlocksCount = functionBlocksCount c - 1, functionScopeEvents = functionScopeEvents c++[newScopeEvent] }
    where newScopeEvent = BlockEnd blockID

gExtrn :: Compiler -> [BName] -> Compiler
gExtrn = foldr declareVarExtrn

gAuto :: Compiler -> [(BName, Maybe Int)] -> Compiler
gAuto = foldr declareVarAuto

gRValue :: Compiler -> BRValue -> Compiler
gRValue c rvalue = allocateAutoVariable 1 c &            -- use AutoVarCount - 1 to accumulate
                   \c' -> case rvalue of

                      FunctionCall f args -> undefined

                      -- RLValue. it takes the arg given by glvalue and assigns it to the acc auto var
                      RLValue a -> gLValue c' a &
                                   \(ar, c'') -> addOp (AutoAssign (cAutoVarCount c) ar) c''

                   & deallocateAutoVariable 1

gLValue :: Compiler -> BLValue -> (Arg, Compiler)
gLValue c l = case l of
                LName n -> let v = findVar (name n) c in if isJust v
                                                         then (case varStorage (fromJust v) of
                                                                StorageExternal s -> External s
                                                                StorageAuto i -> AutoVar i, c)
                                                         else (bogusArg, addError ("Could not find variable '" ++ name n ++ "'") c)

tee = [FDefinition (BName {name = "main", nameLoc = 0}) []
       (Block [
         Extrn [BName {name = "hi", nameLoc = 18}]
        ,SRValue (RLValue (LName (BName {name = "hi", nameLoc = 26})))
        ,Extrn [BName {name = "h", nameLoc = 18}]
        ,SRValue (RLValue (LName (BName {name = "hi", nameLoc = 26})))
        ])]

teee = [FDefinition {fName = BName {name = "main", nameLoc = 0},
                     fArgs = [BName {name = "argc", nameLoc = 5}],
                     fStatement = Block [Auto [(BName {name = "a", nameLoc = 21},Just 10),
                                               (BName {name = "b", nameLoc = 23},Nothing),
                                               (BName {name = "c", nameLoc = 25},Nothing)],
                                         SRValue (RLValue (LName (BName {name = "a", nameLoc = 32})))]}]
prettyier :: (Show a) => a -> IO ()
prettyier s = putStrLn $ snd $
            foldr pick (0,"") $
            show s
    where opening = map (==) "{["
          closing = map (==) "}]"
          pick x (ind, str) | x == ','          = (ind, x:'\n':getInd (ind*2-1)++str)
                            | any ($ x) opening = (ind-1, x:'\n':getInd (ind*2)++str)
                            | any ($ x) closing = (ind+1, x:str)
                            | x == '\n'         = (ind, x:getInd (ind*2)++str)
                            | otherwise         = (ind, x:str)
          getInd i = replicate i ' '
