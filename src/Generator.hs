-- this will generate IR code from AST.

module Generator where

import BParser
import Parser
import Data.Word
import Data.Char
import Data.List
import Data.Maybe
import Data.Function
import Control.Applicative

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

data Storage = StorageExternal String
             | StorageAuto Int
               deriving (Eq, Show)

data Var = Var {
      varName :: String,
      varStorage :: Storage,
      varLoc :: Int
    } deriving (Eq, Show)

data Function = Function {
      funName :: String,
      funLoc :: Int,
      body :: [Op],
      paramsCount :: Word,
      autoVarCount :: Word
    } deriving (Eq, Show)

data IRProgram = IRProgram {
      functions :: [Function],
      staticData :: [Word8],
      extrns :: [String]
    } deriving (Eq, Show)

data GenError = GenError {
      genErrorString :: String,
      genErrorLocLength :: Maybe (Int, Int)
    } deriving (Eq, Show)

data Compiler = Compiler {
      program :: IRProgram,
      errors :: [GenError],

      vars :: [[Var]],

      functionNames :: [BName],

      functionBody :: [Op],
      functionBlocksCount :: Int,

      cAutoVarCount :: Int,
      cAutoVarCountMax :: Int
    } deriving (Eq, Show)

emptyCompiler = Compiler (IRProgram [] [] []) [] [[]] [] [] 0 0 0

initCompiler :: BProgram -> Compiler
initCompiler ast = emptyCompiler { functionNames = map fName ast }

allocateAutoVariable :: Int -> Compiler -> Compiler
allocateAutoVariable sizeToAlloc c =
    c { cAutoVarCount = count,
                        cAutoVarCountMax = max (cAutoVarCountMax c) count }
    where count = cAutoVarCount c + sizeToAlloc

deallocateAutoVariable :: Int -> Compiler -> Compiler
deallocateAutoVariable sizeToDealloc c = c { cAutoVarCount = count}
    where count = cAutoVarCount c - sizeToDealloc

declareVarExtrn :: BName -> Compiler -> Compiler
declareVarExtrn n c = if isNothing (findVar (name n) c)
                      then c { vars = newStack:remainingScopes, program = newProgram }
                      else addError (Just n) (\x -> "Redefinition of variable '" ++ x ++ "'") c
    where newStack = newVar:uppermostScope
          newVar = Var (name n) (StorageExternal (name n)) (nameLoc n)
          remainingScopes = drop 1 (vars c)
          [uppermostScope] = take 1 (vars c)
          newProgram = (program c) { extrns = name n:extrns (program c) }

declareVarAuto :: (BName, Maybe Int) -> Compiler -> Compiler
declareVarAuto (n, size) c = if isNothing (findVar (name n) c)
                             then c' { vars = newStack:remainingScopes }
                             else addError (Just n) (\x -> "Redefinition of variable '" ++ x ++ "'") c
    where newStack = newVar:uppermostScope
          newVar = Var (name n) (StorageAuto (cAutoVarCount c)) (nameLoc n)
          remainingScopes = drop 1 (vars c)
          [uppermostScope] = take 1 (vars c)
          c' = allocateAutoVariable (fromMaybe 1 size) c

addOp :: Op -> Compiler -> Compiler
addOp o c = c { functionBody = functionBody c ++ [o] }

addError :: Maybe BName -> (String -> String) -> Compiler -> Compiler
addError n s c = c { errors = errors c ++ [ne] }
    where ne = if isJust n
               then let n' = fromJust n in GenError (s (name n')) (Just (nameLoc n', length $ name n'))
               else GenError (s "") Nothing

bogusArg = External "bogusArgument"

findVar :: String -> Compiler -> Maybe Var
findVar n c = if null foundVars then Nothing else let (hfv:_) = foundVars in Just hfv
    where findVar = find (\x -> varName x == n)
          foundVars = mapMaybe findVar (vars c)

gProgram :: BProgram -> ([GenError], IRProgram)
gProgram p = (errors c, program c)
    where c = gCompile p

gCompile :: BProgram -> Compiler
gCompile a = foldr gDefinition (initCompiler a) a

gDefinition :: BDefinition -> Compiler -> Compiler
gDefinition (FDefinition name args block) = gFunction name args block

gFunction :: BName -> [BName] -> BStatement -> Compiler -> Compiler
gFunction bname args block c = emptyCompiler { program = newestProgram, errors = errors c', functionNames = functionNames c' }
    where c' = foldl' (flip $ declareVarAuto . (, Nothing)) c args
               & \x -> gStatement x block

          newFunc = Function (name bname)
                    (nameLoc bname)
                    (functionBody c')
                    (fromIntegral $ length args)
                    (fromIntegral $ cAutoVarCountMax c')

          newestProgram = let newProgram = program c' in newProgram { functions = newFunc:functions newProgram }

gStatement :: Compiler -> BStatement -> Compiler
gStatement c statement = case statement of
                               Block   a -> gBlock c a
                               Extrn   a -> gExtrn c a
                               Auto    a -> gAuto c a
                               SRValue a -> let stackSize = cAutoVarCount c in gRValue c a & \(_,c') -> c' { cAutoVarCount = stackSize }

gBlock :: Compiler -> [BStatement] -> Compiler
gBlock c ss = c'' { cAutoVarCount = autoVarC }
    where c' = blockBegin c
          autoVarC = cAutoVarCount c
          c'' = c' & \x -> foldl' gStatement x ss & blockEnd (functionBlocksCount c)

blockBegin :: Compiler -> Compiler
blockBegin c = c { vars = []:vars c, functionBlocksCount = 1+functionBlocksCount c }

blockEnd :: Int -> Compiler -> Compiler
blockEnd blockID c = c { vars = drop 1 $ vars c, functionBlocksCount = functionBlocksCount c - 1 }

gExtrn :: Compiler -> [BName] -> Compiler
gExtrn = foldr declareVarExtrn

gAuto :: Compiler -> [(BName, Maybe Int)] -> Compiler
gAuto = foldr declareVarAuto

gRValue :: Compiler -> BRValue -> (Arg, Compiler)
gRValue c rvalue = case rvalue of
                     FunctionCall f args -> gFunctionCall f args c
                     Assignment l assOp r -> gAssignment c l assOp r
                     RLValue a -> gLValue c a
                     RConstant a -> gConstant c a

gFunctionCall :: BRValue -> [BRValue] -> Compiler -> (Arg, Compiler)
gFunctionCall functionLoc args c = (AutoVar autoVarOffset, addOp newOp c''')
    where c' = allocateAutoVariable 1 c
          (fLocArg, c'') = gRValue c' functionLoc
          (fArgsArg, c''') = foldr (\a (as,x) -> let (na, nx) = gRValue x a in (na:as, nx)) ([], c'') args
          autoVarOffset = fromIntegral $ cAutoVarCount c
          newOp = Funcall autoVarOffset fLocArg fArgsArg

gAssignment :: Compiler -> BLValue -> BAssign -> BRValue -> (Arg, Compiler)
gAssignment c lValue assOp rValue = case assOp of
                                      Assign -> (lArg, addOp newOp c)
    where (rArg, c') = gRValue c rValue
          (lArg, c'') = gLValue c lValue
          newOp = case lArg of
                    External a -> ExternalAssign a rArg
                    AutoVar a -> AutoAssign (fromIntegral a) rArg

gLValue :: Compiler -> BLValue -> (Arg, Compiler)
gLValue c l = case l of
                LName n -> let v = findVar (name n) c in if isJust v
                                                         then (case varStorage (fromJust v) of
                                                                StorageExternal s -> External s
                                                                StorageAuto i -> AutoVar (fromIntegral i), c)
                                                         else (bogusArg, addError (Just n) (\x -> "Could not find variable '" ++ x ++ "'") c)

gConstant :: Compiler -> BConstant -> (Arg, Compiler)
gConstant c constantValue = case constantValue of
                              Digit a -> (Literal $ fromIntegral a, c)
                              Char a -> (Literal $ fromIntegral $ ord a, c)
                              Chars a -> (DataOffset dataLength, c { program = oldProgram { staticData = newStaticData } })
                                  where oldProgram = program c
                                        oldStaticData = staticData oldProgram
                                        newStaticData = oldStaticData ++ currStaticData
                                        Right (currStaticData, _) = startParser escapeChars a
                                        dataLength = fromIntegral $ length oldStaticData

escapeChars :: Parser [Word8]
escapeChars = repeatedParser $
              (charP '*' *> (charEscape <$> pan))
              <|> (fromIntegral . ord <$> pan)
    where pan = predicateP (const True) "Char"
          charEscape c = (fromIntegral . ord) $ case c of
                           '0' -> '\0'
                           '*' -> '*'
                           'n' -> '\n'
              

tee = [FDefinition {fName = BName {name = "main", nameLoc = 0}, fArgs = [BName {name = "argc", nameLoc = 5}], fStatement = Block [Extrn [BName {name = "printf", nameLoc = 22}],SRValue (FunctionCall (RLValue (LName (BName {name = "printf", nameLoc = 34}))) [RConstant (Digit 1)])]}]

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
