-- this will generate IR code from AST.

module Generator where

import BParser
import Control.Applicative
import Data.Char
import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe
import Data.Word
import Parser

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
      nFunName :: String,
      nFunLoc  :: Int,
      nBody    :: [String]
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

data CompilerState = CompilerState {
      program             :: IRProgram,
      errors              :: [GenError],

      vars                :: [[Var]],

      globalNames         :: [BName],

      functionBody        :: [Op],
      functionBlocksCount :: Word,
      functionLabelCount  :: Word,

      cAutoVarCount       :: Word,
      cAutoVarCountMax    :: Word
    } deriving (Eq, Show)

newtype Compiler a = Compiler { runCompiler :: CompilerState -> (CompilerState, a) } deriving (Functor)

instance Applicative Compiler where
    pure a = Compiler (,a)
    (Compiler x) <*> (Compiler y) = Compiler $ \c -> let (cs, f) = x c
                                                         (cs',t) = y cs
                                                     in (cs', f t)

instance Monad Compiler where
    x >>= y = Compiler $ \c -> let (cs, input) = runCompiler x c
                               in runCompiler (y input) cs

setCompiler :: CompilerState -> Compiler ()
setCompiler cs = Compiler $ const (cs,())

getCompiler :: Compiler CompilerState
getCompiler = Compiler $ \cs -> (cs,cs)

updateCompiler :: (CompilerState -> CompilerState) -> Compiler ()
updateCompiler f = Compiler $ \c -> (f c,())

emptyCompiler = CompilerState (IRProgram [] [] [] [] []) [] [[]] [] [] 0 0 0 0

newLabel :: Compiler Word
newLabel = Compiler $ \c -> (c { functionLabelCount = functionLabelCount c + 1 }, functionLabelCount c )

allocateAutoVariable :: Word -> Compiler Word
allocateAutoVariable sizeToAlloc = Compiler $ \c -> let count = cAutoVarCount c + sizeToAlloc
                                                    in (c { cAutoVarCount = count,
                                                            cAutoVarCountMax = max (cAutoVarCountMax c) count },
                                                         cAutoVarCount c)

deallocateAutoVariable :: Word -> Compiler ()
deallocateAutoVariable previousStackSize = updateCompiler $ \c -> c { cAutoVarCount = previousStackSize }

addOp :: Op -> Compiler ()
addOp o = updateCompiler $ \c -> c { functionBody = functionBody c ++ [o] }

addError :: Maybe BName -> (String -> String) -> Compiler ()
addError n s = updateCompiler $ \c -> c { errors = errors c ++ [ne] }
    where ne = if isJust n
               then let n' = fromJust n in GenError (s (name n')) (Just (nameLoc n', length $ name n'))
               else GenError (s "") Nothing

findVar :: String -> Compiler (Maybe Var)
findVar n = do
  cs <- getCompiler
  let foundVars = mapMaybe (find (\x -> varName x == n)) (vars cs)
  return $ if null foundVars then Nothing else let (hfv:_) = foundVars in Just hfv

declareVar :: BName -> Storage -> Compiler ()
declareVar n s = do
  cs <- getCompiler
  let newVar = Var (name n) s (nameLoc n)
  let (uppermostScope:remainingScopes) = vars cs
  redefinition <- findVar (name n)
  if isNothing redefinition
  then setCompiler ( cs { vars = (newVar:uppermostScope):remainingScopes } )
  else addError (Just n) (\x -> "Redefinition of variable '" ++ x ++ "'")

declareVarExtrn :: BName -> Compiler ()
declareVarExtrn n = do
  updateCompiler $ \c -> c { program = (program c) { extrns = name n:extrns (program c) } }
  declareVar n (StorageExternal (name n))

declareVarAuto :: (BName, Maybe Word) -> Compiler ()
declareVarAuto (n, size) = do
  autoVarIndex <- allocateAutoVariable (fromMaybe 1 size)
  declareVar n (StorageAuto autoVarIndex)

blockBegin :: Compiler ()
blockBegin = updateCompiler $ \c -> c { vars = []:vars c, functionBlocksCount = 1+functionBlocksCount c }

blockEnd :: Compiler ()
blockEnd = updateCompiler $ \c -> c { vars = drop 1 $ vars c, functionBlocksCount = functionBlocksCount c - 1 }

bogusArg = External "bogusArgument"

gExtrn :: [BName] -> Compiler ()
gExtrn = traverse_ declareVarExtrn

gAuto :: [(BName, Maybe Word)] -> Compiler ()
gAuto = traverse_ declareVarAuto

initCompiler :: BProgram -> Compiler ()
initCompiler = traverse_ folder
    where folder d = case d of
                       FDefinition fn _ _ -> updateCompiler $ \c -> c { globalNames = fn:globalNames c }
                       GlobalVar vn vs vi -> do
                              ivs <- traverse (\i -> case i of
                                                       IConstant a -> gConstant a
                                                       IName a -> gLValue (LName a)) vi
                              declareVarExtrn vn
                              let newGV = (name vn, vs, ivs)
                              cs <- getCompiler
                              let newProgram = (program cs) { globalVars = newGV:globalVars (program cs) }
                              updateCompiler $ \c -> c { program = newProgram, globalNames = vn:globalNames c }
                       NakedFunction n block -> do
                              declareVarExtrn n
                              updateCompiler $ \c -> let oldP = program c
                                                         newProgram = oldP { nakedFunctions = newNF:nakedFunctions oldP }
                                                         newNF = NFunction (name n) (nameLoc n) block
                                                     in c { program = newProgram, globalNames = n:globalNames c }

gProgram :: BProgram -> ([GenError], IRProgram)
gProgram p = (errors c, program c)
    where (c,_) = runCompiler (initCompiler p >>= const (gCompile p)) emptyCompiler

gCompile :: BProgram -> Compiler ()
gCompile = traverse_ gDefinition

gDefinition :: BDefinition -> Compiler ()
gDefinition (FDefinition name args block) = gFunction name args block
gDefinition (GlobalVar n mc ivals)        = pure ()
gDefinition (NakedFunction n block)       = pure ()

gFunction :: BName -> [BName] -> BStatement -> Compiler ()
gFunction bname args block = do
  traverse_ (declareVarAuto . (,Nothing)) args
  gStatement block
  cs <- getCompiler
  let newFunc = Function (name bname)
                (nameLoc bname)
                (functionBody cs)
                (fromIntegral $ length args)
                (fromIntegral $ cAutoVarCountMax cs)
  let newestProgram = let newProgram = program cs in newProgram { functions = newFunc:functions newProgram }
  updateCompiler $ \c' -> emptyCompiler { program = newestProgram, errors = errors c', globalNames = globalNames c'}

gStatement :: BStatement -> Compiler ()
gStatement statement = case statement of
                         Block   a            -> gBlock a
                         Extrn   a            -> gExtrn a
                         Auto    a            -> gAuto (map (\(x,y)->(x,fmap fromIntegral y)) a)
                         While   cond st      -> gWhile cond st
                         SRValue a            -> do
                                        stackSize <- cAutoVarCount <$> getCompiler
                                        gRValue a
                                        updateCompiler $ \c -> c { cAutoVarCount = stackSize }
                         IfElse  cond tst fst -> gIfElse cond tst fst
                         BReturn (Just a)     -> do
                                        rArg <- gRValue a
                                        addOp (Return $ Just rArg)
                         BReturn Nothing      -> addOp (Return Nothing)
                         InlineAsm a          -> addOp (Asm a)
                         Empty                -> pure ()

gBlock :: [BStatement] -> Compiler ()
gBlock ss = do
  stackSize <- cAutoVarCount <$> getCompiler
  blockBegin
  traverse_ gStatement ss
  blockEnd
  updateCompiler $ \c -> c { cAutoVarCount = stackSize }

gWhile :: BRValue -> BStatement -> Compiler ()
gWhile cond st = do
  label <- newLabel
  addOp (Label label)
  condArg <- gRValue cond
  cs <- getCompiler
  let cs' = cs { functionBody = functionBody cs ++ [JmpIfZeroLabel exitLabel condArg] }
      (cs'', ()) = runCompiler (gStatement st) cs'
      exitLabel = functionLabelCount cs''
      cs''' = cs'' { functionBody = functionBody cs'' ++ [JmpLabel label, Label exitLabel], functionLabelCount = functionLabelCount cs'' + 1 }
  setCompiler cs'''

gIfElse :: BRValue -> BStatement -> Maybe BStatement -> Compiler ()
gIfElse cond tst Nothing = do
  condArg <- gRValue cond
  cs <- getCompiler
  let cs' = cs { functionBody = functionBody cs ++ [JmpIfZeroLabel exitLabel condArg] }
      (cs'', ()) = runCompiler (gStatement tst) cs'
      exitLabel = functionLabelCount cs''
      cs''' = cs'' { functionBody = functionBody cs'' ++ [Label exitLabel] ,functionLabelCount = functionLabelCount cs'' + 1 }
  setCompiler cs'''

gIfElse cond tst (Just fst) = do
  condArg <- gRValue cond
  cs <- getCompiler
  let cs1 = cs { functionBody = functionBody cs ++ [JmpIfZeroLabel enterElseLabel condArg] }
      (cs2, ()) = runCompiler (gStatement tst) cs1
      enterElseLabel = functionLabelCount cs2
      cs3 = cs2 { functionBody = functionBody cs2 ++ [JmpLabel exitAfterElseLabel, Label enterElseLabel] ,functionLabelCount = functionLabelCount cs2 + 1 }
      (cs4, ()) = runCompiler (gStatement fst) cs3
      exitAfterElseLabel = functionLabelCount cs4
      cs5 = cs4 { functionBody = functionBody cs4 ++ [Label exitAfterElseLabel] ,functionLabelCount = functionLabelCount cs4 + 1 }
  setCompiler cs5

gRValue :: BRValue -> Compiler Arg
gRValue rvalue = case rvalue of
                   FunctionCall f args  -> gFunctionCall f args
                   Assignment l assOp r -> gAssignment l assOp r
                   RLValue a            -> gLValue a
                   RConstant a          -> gConstant a
                   Binary l op r        -> gBinary l op r
                   BracketRValue rv     -> gRValue rv
                   IncDecPost l op      -> gIncDec l op True
                   IncDecPre  op l      -> gIncDec l op False
                   RUnary op r          -> gUnary op r
                   Ternary cond t f     -> gTernary cond t f

gFunctionCall :: BRValue -> [BRValue] -> Compiler Arg
gFunctionCall functionLoc args = do
  autoVarOffset <- allocateAutoVariable 1
  fLoc <- gRValue functionLoc
  fArgs <- traverse gRValue args
  addOp (Funcall autoVarOffset fLoc fArgs)
  return (AutoVar autoVarOffset)

gAssignment :: BLValue -> BAssign -> BRValue -> Compiler Arg
gAssignment lValue assign rValue = do
  rArg <- gRValue rValue
  lArg <- gLValue lValue
  case assign of
    Assign -> do
      addOp (case lArg of
               External a -> ExternalAssign a rArg
               AutoVar a  -> AutoAssign (fromIntegral a) rArg
               Deref a    -> MemoryAssign a rArg)
    BinaryAssign bop -> do
                case lArg of
                  AutoVar a -> addOp (OpBin bop a lArg rArg)
                  External a -> do
                            tempStorage <- allocateAutoVariable 1
                            addOp (OpBin bop tempStorage lArg rArg)
                            addOp (ExternalAssign a (AutoVar tempStorage))
  return lArg

wow = runCompiler ( do
                    gLValue (LName (BName "wow" 22)))
      emptyCompiler { globalNames = [BName "wow" 44]}

gLValue :: BLValue -> Compiler Arg
gLValue l = case l of
              LName n -> do
                     vf <- find (\b -> name n == name b) . globalNames <$> getCompiler
                     if isJust vf then return (External (name n))
                     else do
                       v <- findVar (name n)
                       if isJust v then
                           return $ case varStorage (fromJust v) of
                                      StorageExternal s -> External s
                                      StorageAuto i     -> AutoVar i
                       else bogusArg <$ addError (Just n) (\x -> "Could not find variable '" ++ x ++ "'")
              Array ptr offset -> do
                     ptrArg <- gRValue ptr
                     offsetArg <- gRValue offset
                     arrayPtr <- allocateAutoVariable 1
                     addOp (Index arrayPtr ptrArg offsetArg)
                     return $ Deref arrayPtr
              Dereference i -> do
                     derefArg <- gRValue i
                     return $ case derefArg of
                                AutoVar a -> Deref a

gConstant :: BConstant -> Compiler Arg
gConstant constantValue = case constantValue of
                              Digit a -> return $ Literal $ fromIntegral a
                              CharConst a -> return $ Literal $ fromIntegral $ ord a
                              Chars a -> do
                                oldProgram <- program <$> getCompiler
                                let oldStaticData = staticData oldProgram
                                let dataLength = (fromIntegral . length) oldStaticData
                                updateCompiler $ \c -> c { program = oldProgram { staticData = oldStaticData ++ fmap (fromIntegral . ord) a } }
                                return (DataOffset dataLength)

gBinary :: BRValue -> BBinary -> BRValue -> Compiler Arg
gBinary l op r = do
  lArg <- gRValue l
  rArg <- gRValue r
  resultAutoVar <- allocateAutoVariable 1
  addOp (OpBin op resultAutoVar lArg rArg)
  return $ AutoVar resultAutoVar

gUnary :: BUnary -> BRValue -> Compiler Arg
gUnary op r = do
  rArg <- gRValue r
  resultAutoVar <- allocateAutoVariable 1
  addOp ((case op of
           Not      -> UnaryNot
           Negative -> Negate) resultAutoVar rArg)
  return $ AutoVar resultAutoVar

gTernary :: BRValue -> BRValue -> BRValue -> Compiler Arg
gTernary cond t f = do
  resultAutoVar <- allocateAutoVariable 1
  condArg <- gRValue cond
  cs <- getCompiler
  let cs1 = cs { functionBody = functionBody cs ++ [JmpIfZeroLabel falseLabel condArg] }
      (cs2, tArg) = runCompiler (gRValue t) cs1
      falseLabel = functionLabelCount cs2
      cs3 = cs2 { functionBody = functionBody cs2 ++ [AutoAssign resultAutoVar tArg, JmpLabel exitFalseLabel, Label falseLabel], functionLabelCount = functionLabelCount cs2 + 1 }
      (cs4, fArg) = runCompiler (gRValue f) cs3
      exitFalseLabel = functionLabelCount cs4
      cs5 = cs4 { functionBody = functionBody cs4 ++ [AutoAssign resultAutoVar fArg, Label exitFalseLabel], functionLabelCount = functionLabelCount cs4 + 1 }
  setCompiler cs5
  return (AutoVar resultAutoVar)

{-
gTernary cond t f c = (AutoVar (cAutoVarCount c' - 1), c''''')
    where (condArg, c') = gRValue (allocateAutoVariable 1 c) cond
          (tArg, c'') = gRValue (addOp (JmpIfZeroLabel (functionLabelCount c''') condArg) c') t
          c''' = (\x -> (addOp (Label (functionLabelCount x)) x)) $ (addOp (JmpLabel (functionLabelCount c''''))) $ (addOp (AutoAssign (cAutoVarCount c' - 1) tArg) c'')
          (fArg, c'''') = gRValue (newLabel (c''' { cAutoVarCount = cAutoVarCount c' })) f
          c''''' = newLabel $ (addOp (Label (functionLabelCount c''''))) $ (addOp (AutoAssign (cAutoVarCount c' - 1) fArg) c'''')
-}

gIncDec :: BLValue -> BIncDec -> Bool -> Compiler Arg
gIncDec l op post = do
  lArg <- gLValue l
  let o = case op of Increment -> Add
                     Decrement -> Subtract
  if post
  then do
    resultAutoVar <- allocateAutoVariable 1
    addOp (AutoAssign resultAutoVar lArg)
    gAssignment l (BinaryAssign o) (RConstant $ Digit 1)
  else gAssignment l (BinaryAssign o) (RConstant $ Digit 1)

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
