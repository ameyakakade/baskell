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
         | External    String
         | Literal     Word -- has to be word
         | DataOffset  Word
           deriving (Eq, Show)

type BinOp = BBinary

data Op = UnaryNot        Word   Arg          -- result, arg
        | Negate          Word   Arg          -- result, arg
        | OpBin           BinOp  Word Arg Arg -- binop, index, lhs, rhs
        | Index           Word   Arg  Arg     -- result, arg, offset
        | AutoAssign      Word   Arg          -- index, arg
        | ExternalAssign  String Arg          -- name, arg
        | MemoryAssign    Word   Arg          -- auto var index to be dereferenced, arg
        | Funcall         Word   Arg  [Arg]   -- result, fn, args
        | Label           Word                -- label
        | JmpLabel        Word                -- label index
        | JmpIfZeroLabel  Word   Arg          -- label index, arg
        | Return          (Maybe Arg)         -- arg
        | Asm             [String]            -- arg
          deriving (Eq, Show)

data Storage = StorageExternal String
             | StorageAuto Word
               deriving (Eq, Show)

data Var = Var {
      varName    :: String,
      varStorage :: Storage,
      varLoc     :: Int
    } deriving (Eq, Show)

data Function = Function {
      funName      :: String,
      funLoc       :: Int,
      body         :: [Op],
      paramsCount  :: Word,
      autoVarCount :: Word
    } deriving (Eq, Show)

data NFunction = NFunction {
      nFunName      :: String,
      nFunLoc       :: Int,
      nBody         :: [String]
    } deriving (Eq, Show)

data IRProgram = IRProgram {
      functions      :: [Function],
      nakedFunctions :: [NFunction],
      staticData     :: [Word8],
      globalVars     :: [(String, Maybe Int, [Arg])],
      extrns         :: [String]
    } deriving (Eq, Show)

data GenError = GenError {
      genErrorString    :: String,
      genErrorLocLength :: Maybe (Int, Int)
    } deriving (Eq, Show)

data Compiler = Compiler {
      program :: IRProgram,
      errors  :: [GenError],

      vars :: [[Var]],

      globalNames  :: [BName],

      functionBody        :: [Op],
      functionBlocksCount :: Word,
      functionLabelCount  :: Word,

      cAutoVarCount    :: Word,
      cAutoVarCountMax :: Word
    } deriving (Eq, Show)

emptyCompiler = Compiler (IRProgram [] [] [] [] []) [] [[]] [] [] 0 0 0 0

initCompiler :: BProgram -> Compiler
initCompiler = foldl' folder emptyCompiler
    where folder c d = case d of
                         FDefinition fn _ _ -> c { globalNames = fn:globalNames c }
                         GlobalVar vn vs vi -> let (ivs, c') =
                                                       foldr (\g (gs, x) -> case g of
                                                                              IConstant a -> let (na, x') = gConstant x a
                                                                                             in (na:gs, x')
                                                                              IName a -> let (na, x') = gLValue x (LName a)
                                                                                         in (na:gs, x')
                                                                              ) ([], c) vi
                                                   newGV = (name vn, vs, ivs)
                                                   newP = (program c') { globalVars = newGV:globalVars (program c') }
                                               in (declareVarExtrn vn c') { program = newP, globalNames = vn:globalNames c' }
                         NakedFunction n block -> (declareVarExtrn n c) { program = newProgram, globalNames = n:(globalNames c) }
                             where oldP = program c
                                   newProgram = oldP { nakedFunctions = newNF:(nakedFunctions oldP) }
                                   newNF = NFunction (name n) (nameLoc n) block

newLabel :: Compiler -> Compiler
newLabel c = c { functionLabelCount = functionLabelCount c + 1 }

allocateAutoVariable :: Int -> Compiler -> Compiler
allocateAutoVariable sizeToAlloc c =
    c { cAutoVarCount = count,
                        cAutoVarCountMax = max (cAutoVarCountMax c) count }
    where count = cAutoVarCount c + fromIntegral sizeToAlloc

deallocateAutoVariable :: Int -> Compiler -> Compiler
deallocateAutoVariable sizeToDealloc c = c { cAutoVarCount = count}
    where count = cAutoVarCount c - fromIntegral sizeToDealloc

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
gDefinition (GlobalVar n mc ivals) = id
gDefinition (NakedFunction n block) = id

gFunction :: BName -> [BName] -> BStatement -> Compiler -> Compiler
gFunction bname args block c = emptyCompiler { program = newestProgram, errors = errors c', globalNames = globalNames c'}
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
                               Block   a            -> gBlock c a
                               Extrn   a            -> gExtrn c a
                               Auto    a            -> gAuto c a
                               While   cond st      -> gWhile c cond st
                               SRValue a            -> let stackSize = cAutoVarCount c in gRValue c a & \(_,c') -> c' { cAutoVarCount = stackSize }
                               IfElse  cond tst fst -> gIfElse c cond tst fst
                               BReturn (Just a)     -> addOp newOp c'
                                                           where (rArg, c') = gRValue c a
                                                                 newOp = Return $ Just rArg
                               BReturn Nothing      -> addOp (Return Nothing) c
                               InlineAsm a          -> addOp (Asm a) c
                               Empty                -> c

gBlock :: Compiler -> [BStatement] -> Compiler
gBlock c ss = c'' { cAutoVarCount = autoVarC }
    where c' = blockBegin c
          autoVarC = cAutoVarCount c
          c'' = c' & \x -> foldl' gStatement x ss & blockEnd (fromIntegral $ functionBlocksCount c)

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
                     FunctionCall f args  -> gFunctionCall f args c
                     Assignment l assOp r -> gAssignment c l assOp r
                     RLValue a            -> gLValue c a
                     RConstant a          -> gConstant c a
                     Binary l op r        -> gBinary l op r c
                     BracketRValue rv     -> gRValue c rv
                     IncDecPost l op      -> gIncDec l op True c
                     IncDecPre  op l      -> gIncDec l op False c
                     RUnary op r          -> gUnary op r c

gFunctionCall :: BRValue -> [BRValue] -> Compiler -> (Arg, Compiler)
gFunctionCall functionLoc args c = (AutoVar autoVarOffset, addOp newOp c''')
    where c' = allocateAutoVariable 1 c
          (fLocArg, c'') = gRValue c' functionLoc
          (fArgsArg, c''') = foldr (\a (as,x) -> let (na, nx) = gRValue x a in (na:as, nx)) ([], c'') args
          autoVarOffset = fromIntegral $ cAutoVarCount c
          newOp = Funcall autoVarOffset fLocArg fArgsArg

gAssignment :: Compiler -> BLValue -> BAssign -> BRValue -> (Arg, Compiler)
gAssignment c lValue Assign rValue = (lArg, addOp newOp c'')
    where (rArg, c') = gRValue c rValue
          (lArg, c'') = gLValue c' lValue
          newOp = case lArg of
                    External a -> ExternalAssign a rArg
                    AutoVar a -> AutoAssign (fromIntegral a) rArg
                    Deref a -> MemoryAssign a rArg
gAssignment c lValue (BinaryAssign bop) rValue = (lArg, c''')
    where (rArg, c') = gRValue c rValue
          (lArg, c'') = gLValue c' lValue
          c''' = case lArg of
                    AutoVar a -> addOp (OpBin bop a lArg rArg) c''
                    External a -> addOp (ExternalAssign a (AutoVar (cAutoVarCount c''))) $
                                  addOp (OpBin bop (cAutoVarCount c'') lArg rArg)
                                  (allocateAutoVariable 1 c'')

gLValue :: Compiler -> BLValue -> (Arg, Compiler)
gLValue c l = case l of
                LName n -> let vf = find (\b->name n==name b) (globalNames c) in if isJust vf then (External (name n), c) else
                           let v = findVar (name n) c in if isJust v
                                                         then (case varStorage (fromJust v) of
                                                                 StorageExternal s -> External s
                                                                 StorageAuto i -> AutoVar (fromIntegral i), c)
                                                         else (bogusArg, addError (Just n) (\x -> "Could not find variable '" ++ x ++ "'") c)
                Array ptr offset -> let (ptrArg, c') = gRValue c ptr
                                        (offsetArg, c'') = gRValue c' offset
                                    in (Deref (cAutoVarCount c''), addOp (Index (cAutoVarCount c'') ptrArg offsetArg) (allocateAutoVariable 1 c''))
                Dereference i    -> let (derefArg, c') = gRValue c i
                                    in case derefArg of
                                         AutoVar a -> (Deref a, c')

gConstant :: Compiler -> BConstant -> (Arg, Compiler)
gConstant c constantValue = case constantValue of
                              Digit a -> (Literal $ fromIntegral a, c)
                              CharConst a -> (Literal $ fromIntegral $ ord a, c)
                              Chars a -> (DataOffset dataLength, c { program = oldProgram { staticData = newStaticData } })
                                  where oldProgram = program c
                                        oldStaticData = staticData oldProgram
                                        newStaticData = oldStaticData ++ fmap (fromIntegral . ord) a
                                        dataLength = fromIntegral $ length oldStaticData

gBinary :: BRValue -> BBinary -> BRValue -> Compiler -> (Arg, Compiler)
gBinary l op r c = (AutoVar resultAutoVar, addOp newOp $ allocateAutoVariable 1 c'')
    where (lArg, c') = gRValue c l
          (rArg, c'') = gRValue c' r
          resultAutoVar = cAutoVarCount c''
          newOp = OpBin op resultAutoVar lArg rArg

gUnary :: BUnary -> BRValue -> Compiler -> (Arg, Compiler)
gUnary op r c = (AutoVar resultAutoVar, addOp newOp $ allocateAutoVariable 1 c')
    where (rArg, c') = gRValue c r
          resultAutoVar = cAutoVarCount c'
          newOp = (case op of
                     Not -> UnaryNot
                     Negative -> Negate
                  ) resultAutoVar rArg

gIncDec :: BLValue -> BIncDec -> Bool -> Compiler -> (Arg, Compiler)
gIncDec l op post c = if post
                      then case op of
                             Increment -> (AutoVar (cAutoVarCount c'), snd $ f Add l $ addOp (AutoAssign (cAutoVarCount c') varL) (allocateAutoVariable 1 c'))
                             Decrement -> (AutoVar (cAutoVarCount c'), snd $ f Subtract l $ addOp (AutoAssign (cAutoVarCount c') varL) (allocateAutoVariable 1 c'))
                      else case op of
                             Increment -> f Add l c
                             Decrement -> f Subtract l c
    where (varL, c') = gLValue c l
          f a loo coo = gAssignment coo loo (BinaryAssign a) (RConstant $ Digit 1)

gWhile :: Compiler -> BRValue -> BStatement -> Compiler
gWhile c cond st = newLabel $ addOp (Label (functionLabelCount c''')) c'''
    where c' = newLabel $ addOp (Label (functionLabelCount c)) c
          (arg, c'') = gRValue c' cond
          newOp = JmpIfZeroLabel (functionLabelCount c''') arg
          c''' = addOp (JmpLabel $ functionLabelCount c) $ gStatement (addOp newOp c'') st

gIfElse :: Compiler -> BRValue -> BStatement -> Maybe BStatement -> Compiler
gIfElse c cond tst Nothing = c''
    where (arg, c') = gRValue c cond
          newOp = JmpIfZeroLabel (functionLabelCount c') arg
          c'' = newLabel $ addOp (Label (functionLabelCount c')) $ gStatement (addOp newOp c') tst

gIfElse c cond tst (Just fst) = newLabel (addOp (Label afterElseLabel) c''')
    where (arg, c') = gRValue c cond
          elseLabel = functionLabelCount c''
          afterElseLabel = functionLabelCount c'''
          c'' = addOp (JmpLabel afterElseLabel) $
                gStatement (addOp (JmpIfZeroLabel elseLabel arg) c') tst
          c''' = gStatement (newLabel (addOp (Label elseLabel) c'')) fst

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
