-- this will generate IR code from AST.

module Generator where

import BParser
import Control.Applicative
import Control.Monad.Fix
import Data.Char
import Data.Foldable       (traverse_)
import Data.Function
import Data.List
import Data.Maybe
import Data.Word

data Arg = AutoVar     Word
         | Deref       Word
         | Ref         Word   -- get address of auto variable
         | RefExternal String -- get address of external variable
         | External    String
         | Literal     Word   -- has to be word
         | DataOffset  Word
         | Dummy
         deriving (Eq, Show)

data NoOps = UpdateStack Word -- stack size
           | Comment String
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
        | NoOp            NoOps               -- operations to pass some info
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
      paramsCount  :: Int,
      autoVarCount :: Int
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
      extrns         :: [String],
      variadics      :: [(String, Int)]
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
      switchStack         :: [[(BConstant, Word)]], -- (constant, label to jump to)

      cAutoVarCount       :: Word,
      cAutoVarCountMax    :: Word
      } deriving (Eq, Show)

emptyCompiler = CompilerState (IRProgram [] [] [] [] [] [("printf", 1)]) [] [[]] [] [] 0 0 [] 0 0

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

newLabel :: Compiler Word
newLabel = Compiler $ \c -> (c { functionBody = functionBody c ++ [Label (functionLabelCount c)], functionLabelCount = functionLabelCount c + 1 }, functionLabelCount c )

getLabel :: Compiler Word
getLabel = Compiler $ \c -> (c { functionLabelCount = functionLabelCount c + 1 }, functionLabelCount c )

setLabel :: Word -> Compiler ()
setLabel lc = Compiler $ \c -> (c { functionBody = functionBody c ++ [Label lc] }, ())

allocateAutoVariable :: Word -> Compiler Word
allocateAutoVariable sizeToAlloc = Compiler $ \c -> let count = cAutoVarCount c + sizeToAlloc
                                                    in (c { cAutoVarCount = count,
                                                            cAutoVarCountMax = max (cAutoVarCountMax c) count },
                                                         cAutoVarCount c)

getStackSize :: Compiler Word
getStackSize = cAutoVarCount <$> getCompiler

updateStack :: Word -> Compiler ()
updateStack previousStackSize = do
    let op = NoOp (UpdateStack previousStackSize)
    c <- getCompiler
    updateCompiler $ \c -> c { cAutoVarCount = previousStackSize }
    if not (null (functionBody c)) && (last (functionBody c) == op)
      then return ()
      else do
        addOp op

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
    addOp (NoOp $ Comment $ "Added variable " ++ name n ++ " at " ++ show autoVarIndex)

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
                       VariadicFunction n minArgs -> do
                           updateCompiler $ \c -> let oldP = program c
                                                      newProgram = oldP { variadics = (name n, minArgs):variadics oldP }
                                                  in c { program = newProgram }

gProgram :: BProgram -> ([GenError], IRProgram)
gProgram p = (errors c, program c)
    where (c,_) = runCompiler (initCompiler p >>= const (gCompile p)) emptyCompiler

gCompile :: BProgram -> Compiler ()
gCompile = traverse_ gDefinition

gDefinition :: BDefinition -> Compiler ()
gDefinition (FDefinition name args block) = gFunction name args block
gDefinition (GlobalVar {})                = pure ()
gDefinition (NakedFunction _ _)           = pure ()
gDefinition (VariadicFunction _ _ )       = pure ()

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
gStatement statement = do
    case statement of
      Block   a                -> gBlock a
      Extrn   a                -> gExtrn a
      Auto    a                -> gAuto (map (\(x,y)->(x,fmap fromIntegral y)) a)
      While   cond st          -> gWhile cond st
      SRValue a                -> do
                     stackSize <- getStackSize
                     gRValue a
                     updateStack stackSize
      IfElse  cond tst fst     -> gIfElse cond tst fst
      BReturn (Just a)         -> do
                     rArg <- gRValue a
                     addOp (Return $ Just rArg)
      BReturn Nothing          -> addOp (Return Nothing)
      InlineAsm a              -> addOp (Asm a)
      Switch expr block        -> gSwitch expr block
      Case loc const statement -> gCase loc const statement
      Empty                    -> pure ()

gBlock :: [BStatement] -> Compiler ()
gBlock ss = do
    stackSize <- getStackSize
    blockBegin
    traverse_ gStatement ss
    blockEnd
    updateStack stackSize

gWhile :: BRValue -> BStatement -> Compiler ()
gWhile cond st = do
    ss <- getStackSize
    label <- newLabel
    condArg <- gRValue cond
    exitLabel <- getLabel
    addOp (JmpIfZeroLabel exitLabel condArg)
    updateStack ss

    gStatement st
    addOp (JmpLabel label)
    setLabel exitLabel
    updateStack ss

gIfElse :: BRValue -> BStatement -> Maybe BStatement -> Compiler ()
gIfElse cond tst Nothing = do
    ss <- getStackSize
    condArg <- gRValue cond
    exitLabel <- getLabel
    addOp (JmpIfZeroLabel exitLabel condArg)
    updateStack ss

    gStatement tst
    setLabel exitLabel
    updateStack ss

gIfElse cond tst (Just fst) = do
    ss <- getStackSize
    condArg <- gRValue cond
    enterElseLabel <- getLabel
    addOp (JmpIfZeroLabel enterElseLabel condArg)
    updateStack ss

    gStatement tst
    updateStack ss
    exitAfterElseLabel <- getLabel
    addOp (JmpLabel exitAfterElseLabel)
    setLabel enterElseLabel

    gStatement fst
    setLabel exitAfterElseLabel
    updateStack ss

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
                   GetAddress lValue    -> getAddress lValue

gFunctionCall :: BRValue -> [BRValue] -> Compiler Arg
gFunctionCall functionLoc args = do
    autoVarOffset <- allocateAutoVariable 1
    ss <- getStackSize
    fLoc <- gRValue functionLoc
    fArgs <- traverse gRValue args
    addOp (Funcall autoVarOffset fLoc fArgs)
    updateStack ss
    return (AutoVar autoVarOffset)

gAssignment :: BLValue -> BAssign -> BRValue -> Compiler Arg
gAssignment lValue assign rValue = do
    ss <- getStackSize
    tempStorage <- allocateAutoVariable 1
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
                              addOp (OpBin bop tempStorage lArg rArg)
                              addOp (ExternalAssign a (AutoVar tempStorage))
    updateStack ss
    return lArg

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
                     arrayPtr <- allocateAutoVariable 1
                     ss <- getStackSize
                     ptrArg <- gRValue ptr
                     offsetArg <- gRValue offset
                     addOp (Index arrayPtr ptrArg offsetArg)
                     updateStack ss
                     return $ Deref arrayPtr
              Dereference i -> do
                     derefArg <- gRValue i
                     case derefArg of
                       AutoVar a -> return $ Deref a
                       Ref a -> return $ AutoVar a
                       _ -> do
                           autoVarOffset <- allocateAutoVariable 1
                           addOp (AutoAssign autoVarOffset derefArg)
                           return (Deref autoVarOffset)

gConstant :: BConstant -> Compiler Arg
gConstant constantValue = case constantValue of
                              Digit a -> return $ Literal $ fromIntegral a
                              CharConst a -> return $ Literal $ fromIntegral $ ord a
                              HexConst a -> return $ Literal $ fromIntegral $ stringToHex a
                              Chars a -> do
                                oldProgram <- program <$> getCompiler
                                let oldStaticData = staticData oldProgram
                                let dataLength = (fromIntegral . length) oldStaticData
                                updateCompiler $ \c -> c { program = oldProgram { staticData = oldStaticData ++ fmap (fromIntegral . ord) a ++ [0] } }
                                return (DataOffset dataLength)

stringToHex :: String -> Int
stringToHex [n] = charToHex n
stringToHex (n:ns) = (charToHex n)*16 + (stringToHex ns)

charToHex :: Char -> Int
charToHex c = if c > '9'
              then if c > 'F'
                   then ord c - ord 'a' + 10
                   else ord c - ord 'A' + 10
              else ord c - ord '0'

gBinary :: BRValue -> BBinary -> BRValue -> Compiler Arg
gBinary l op r = do
    resultAutoVar <- allocateAutoVariable 1
    ss <- getStackSize
    lArg <- gRValue l
    rArg <- gRValue r
    addOp (OpBin op resultAutoVar lArg rArg)
    updateStack ss
    return $ AutoVar resultAutoVar

gUnary :: BUnary -> BRValue -> Compiler Arg
gUnary op r = do
    resultAutoVar <- allocateAutoVariable 1
    ss <- getStackSize
    rArg <- gRValue r
    addOp ((case op of
             Not      -> UnaryNot
             Negative -> Negate) resultAutoVar rArg)
    updateStack ss
    return $ AutoVar resultAutoVar

gTernary :: BRValue -> BRValue -> BRValue -> Compiler Arg
gTernary cond t f = do
    resultAutoVar <- allocateAutoVariable 1
    ss <- getStackSize
    condArg <- gRValue cond
    falseLabel <- getLabel
    addOp (JmpIfZeroLabel falseLabel condArg)
    tArg <- gRValue t
    addOp (AutoAssign resultAutoVar tArg)
    exitFalseLabel <- getLabel
    addOp (JmpLabel exitFalseLabel)
    setLabel falseLabel
    fArg <- gRValue f
    addOp (AutoAssign resultAutoVar fArg)
    setLabel exitFalseLabel
    cs <- getCompiler
    updateStack ss
    return (AutoVar resultAutoVar)

gIncDec :: BLValue -> BIncDec -> Bool -> Compiler Arg
gIncDec l op post = do
    let o = case op of Increment -> Add
                       Decrement -> Subtract
    if post
      then do
        lArg <- gLValue l
        resultAutoVar <- allocateAutoVariable 1
        addOp (AutoAssign resultAutoVar lArg)
        gAssignment l (BinaryAssign o) (RConstant $ Digit 1)
        return (AutoVar resultAutoVar)
      else gAssignment l (BinaryAssign o) (RConstant $ Digit 1)

gSwitch :: BRValue -> BStatement -> Compiler ()
gSwitch expr statement = do
    exprArg <- gRValue expr
    enterSwitch <- getLabel
    addOp (JmpLabel enterSwitch)
    updateCompiler (\c -> c { switchStack = []:switchStack c})
    gStatement statement
    outLabel <- getLabel
    addOp (JmpLabel outLabel)
    setLabel enterSwitch
    c <- getCompiler
    let cases = head $ switchStack c
    traverse_ (gCompareCase exprArg) cases
    setLabel outLabel
    updateCompiler (\c -> c { switchStack = drop 1 $ switchStack c})

gCompareCase :: Arg -> (BConstant, Word) -> Compiler ()
gCompareCase exprArg (const, label) = do
    stackSize <- getStackSize
    constArg <- gConstant const
    resultAutoVar <- allocateAutoVariable 1
    addOp (OpBin NotEqual resultAutoVar exprArg constArg)
    addOp (JmpIfZeroLabel label (AutoVar resultAutoVar))
    updateStack stackSize

gCase :: Int -> BConstant -> BStatement -> Compiler ()
gCase loc const statement = do
    c <- getCompiler
    if null $ switchStack c
      then updateCompiler (\c -> c { errors = errors c ++ [GenError ("Case " ++ show const ++ " is not inside a switch statement.") (Just (loc, 0))] })
      else do
        label <- newLabel
        let clpair = (const, label)
        updateCompiler (\c -> let ss = switchStack c in c { switchStack = (clpair:head ss):(drop 1 $ ss)})
        gStatement statement

getAddress :: BLValue -> Compiler Arg
getAddress (Dereference rVal) = gRValue rVal
getAddress lValue = do
    lArg <- gLValue lValue
    case lArg of
      External a -> return (RefExternal a)
      AutoVar a  -> return (Ref a)
      Deref a    -> return (AutoVar a)

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
