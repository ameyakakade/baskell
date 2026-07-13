{-# LANGUAGE OverloadedStrings #-}
module Codegen.GasAArch64 where

import           BParser        (BBinary (..))
import           Codegen.Common
import           Control.Monad
import           Data.Bits
import           Data.Foldable
import           Data.List
import           Data.Maybe
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import           Data.Word
import           Generator

data State = State {
    asmOutput           :: T.Text,
    count               :: Word,
    registerStates      :: [(Word, Arg, Word)], -- Register, arg, age
    firstTouchToAutoVar :: [Int], -- list of auto vars not yet initialized
    codegenLog          :: [T.Text],
    aVariadics          :: [(T.Text, Int)]
    } deriving (Show)

type RegCodegen = StateM State

tShow :: Show a => a -> T.Text
tShow a = T.pack $ show a

addCount :: RegCodegen ()
addCount = StateM $ \s -> (s { count = count s + 1, codegenLog = codegenLog s <>
                               ["Count: " <> (T.pack $ show (count s)) <> "\t" <>
                                (let a = (T.pack $ show (registerStates s)) in "Reg " <> a <> T.replicate (135 - T.length a) (T.singleton ' ')) <> "\t" <>

                                (let a = asmOutput s in if T.null a then "" else "")]},())

setState :: State -> RegCodegen ()
setState s = StateM (const (s,())) >>= const addCount

getState :: RegCodegen State
getState = addCount >>= const (StateM $ \s -> (s,s))

updateState :: (State -> State) -> RegCodegen ()
updateState f = addCount >>= const (StateM $ \s -> (f s,()))

append :: T.Text -> RegCodegen ()
append ins = updateState $ \s -> s { asmOutput = asmOutput s <> ins <> "\n" }

gasAArch64 = Target "gasAArch64" False asm

asm :: IRProgram -> IO T.Text
asm p = do
    let s = fst $ runStateM (generateAsm p) (State T.empty 0 [] [] [] (variadics p))
    when False
      $ do
        print $ count s
        print $ registerStates s
        print $ firstTouchToAutoVar s
        putStrLn "---"
        T.putStr $ T.unlines $ codegenLog s
        putStrLn "---"
    return $ asmOutput s

generateAsm :: IRProgram -> RegCodegen ()
generateAsm p = do
    aProgramPrologue
    traverse_ aFunction (functions p)
    aGlobalVarSection (globalVars p)
    traverse_ aNakedFunctionSection (nakedFunctions p)
    aDataSection (staticData p)

aProgramPrologue :: RegCodegen ()
aProgramPrologue = do
    append ".text"

aGlobalVarSection :: [(T.Text, Maybe Int, [Arg])] -> RegCodegen ()
aGlobalVarSection l = do
    traverse_ (\(s, ms, args) ->
                if isNothing ms
                then aGlobalVar s args
                else aGlobalVector s (fromJust ms) args
             ) l

aGlobalVar :: T.Text -> [Arg] -> RegCodegen ()
aGlobalVar vName initData = do
    append $ ".data"
    append $ ".global _" <> vName
    append $ ".p2align 3 // investigate why this is needed"
    append $ "_" <> vName <> ":"
    let as = if null initData
             then [".quad 0"]
             else map (\a -> ".quad " <> aGlobalVarArg a) initData
    updateState $ \s -> s { asmOutput = asmOutput s <> T.unlines as }

aGlobalVector :: T.Text -> Int -> [Arg] -> RegCodegen ()
aGlobalVector vName vSize initData = undefined

aDataSection :: [Word8] -> RegCodegen ()
aDataSection a = do
    append $ ".data"
    append $ ".dat: .byte " <> T.intercalate "," (map tShow a)

aGlobalVarArg :: Arg -> T.Text
aGlobalVarArg (External a)   = "_" <> a
aGlobalVarArg (Literal a)    = tShow a
aGlobalVarArg (DataOffset a) = ".dat +" <> tShow a

aNakedFunctionSection :: NFunction -> RegCodegen ()
aNakedFunctionSection (NFunction nfName nfLoc nfBlock) = do
    append $  ".global _" <> nfName
    append $  ".p2align 4"
    append $ "_" <> nfName <> ":"
    append $ T.unlines nfBlock

storeVarOnStack :: Int -> Int -> RegCodegen ()
storeVarOnStack reg offset = append $ "STR X" <> tShow reg <> ", [FP, #" <> tShow (offset*8) <> "]"

loadVarInStack :: Word -> Word -> RegCodegen ()
loadVarInStack destReg offset = append $ "LDR X" <> tShow destReg <> ", [FP, #" <> tShow (offset*8) <> "]"

storeVarInMem :: Word -> Word -> RegCodegen ()
storeVarInMem reg ptrReg = do
    append $ "STR X" <> tShow reg <> ", [X" <> tShow ptrReg <> ", #0]"
    append $ "; storing variable in memory"

loadVarInMem :: Word -> Word -> RegCodegen ()
loadVarInMem destReg ptrReg = do
    append $ "LDR X" <> tShow destReg <> ", [X" <> tShow ptrReg <> ", #0]"
    append $ "; loading variable in memory"

saveRegisters :: RegCodegen ()
saveRegisters = do
    s <- getState
    traverse_ saveOneRegister (registerStates s)
    updateState $ \s -> s { registerStates = [] }

saveOneRegister :: (Word, Arg, Word) -> RegCodegen ()
saveOneRegister (register, arg, age) = case arg of
                                         AutoVar offset -> storeVarOnStack (fromIntegral register) (fromIntegral offset)
                                         _ -> return ()

findArgInRegisters :: Arg -> RegCodegen (Maybe Word)
findArgInRegisters arg = do
    s <- getState
    return $ (\(a,_,_) -> a) <$> find (\(_,a,_) -> a==arg) (registerStates s)

getEmptyRegister :: RegCodegen Word
getEmptyRegister = do
    s <- fmap registerStates getState
    let l = length s
    if l >= 16
      then do
        undefined
      else do
        let availableRegisters = [x | x<-[0..16], x `notElem` map (\(x, _, _) -> x) s]
        return $ head availableRegisters

addRegisterCache :: Arg -> Word -> RegCodegen ()
addRegisterCache arg register = updateState $ \s -> s { registerStates = (register, arg, count s):registerStates s }

isVarTouched :: Word -> RegCodegen Bool
isVarTouched autoVar = do
    s <- getState
    let untouched = fromIntegral autoVar `elem` firstTouchToAutoVar s
    if untouched
      then do
        updateState $ \s -> s { firstTouchToAutoVar = delete (fromIntegral autoVar) (firstTouchToAutoVar s) }
        return False
      else return True

aFunction :: Function -> RegCodegen ()
aFunction f = do
    aFunctionPrologue (funName f) (paramsCount f) (autoVarCount f)
    updateState $ \s -> s { registerStates = [], firstTouchToAutoVar = [(paramsCount f)..(paramsCount f + autoVarCount f - 1)] }
    traverse_ (aOp (funName f) (paramsCount f) (autoVarCount f)) (body f)
    aFunctionEpilogue (paramsCount f) (autoVarCount f)
    append ""
    return ()

aFunctionPrologue :: T.Text -> Int -> Int -> RegCodegen ()
aFunctionPrologue name countParam countAutoVars = do
    append $ ".global _" <> name
    append $ ".p2align 4"
    append $ "_" <> name <> ":"
    append $ "STP LR, FP, [SP, #-16]!"
    append $ "SUB SP, SP, #" <> tShow (alignStackOffset $ (countParam + countAutoVars)*8)
    append $ "MOV FP, SP"
    if countParam == 0
      then return ()
      else zipWithM_ storeVarOnStack [0..(countParam - 1)] [0..(countParam - 1)]

alignStackOffset ccc = if mod ccc 16 == 0 then ccc else div ccc 16*16 + 16

aFunctionEpilogue :: Int -> Int -> RegCodegen ()
aFunctionEpilogue countParam countAutoVars = do
    append $ "ADD SP, SP, #" <> tShow (alignStackOffset $ (countParam + countAutoVars)*8)
    append $ "LDP LR, FP, [SP], #16"
    append $ "RET"

aArg :: Arg -> RegCodegen Word
aArg arg = do
    maybeInRegister <- findArgInRegisters arg
    maybe
      (case arg of
             DataOffset doff -> do
                 r <- getEmptyRegister
                 loadArgIntoReg r arg
                 addRegisterCache arg r
                 return r
             Literal a -> do
                 r <- getEmptyRegister
                 loadArgIntoReg r arg
                 addRegisterCache arg r
                 return r

             AutoVar autoVarOffset -> do
                 r <- getEmptyRegister
                 loadArgIntoReg r arg
                 addRegisterCache arg r
                 return r
             Deref autoVarOffset -> do
                 autoVarReg <- aArg (AutoVar autoVarOffset)
                 r <- getEmptyRegister
                 loadVarInMem r autoVarReg
                 return r
             External name -> do
                 r <- getEmptyRegister
                 append $ "ADRP X" <> tShow r <> ", _" <> name <> "@GOTPAGE"
                 append $ "LDR X" <> tShow r <> ", [X" <> tShow r <> ", _" <> name <> "@GOTPAGEOFF]"
                 append $ "LDR X" <> tShow r <> ", [X" <> tShow r <> "]\n"
                 addRegisterCache arg r
                 return r
             Ref offset -> do
                 saveRegisters -- TODO: Maybe only discard the auto var being dereferenced.
                 r <- getEmptyRegister
                 append $ "MOV X" <> tShow r <> ", FP"
                 append $ "ADD X" <> tShow r <> ", X" <> tShow r <> ", #" <> tShow (offset*8)
                 return r
             RefExternal name -> do
                 r <- getEmptyRegister
                 append $ "ADRP X" <> tShow r <> ", _" <> name <> "@GOTPAGE"
                 append $ "LDR X" <> tShow r <> ", [X" <> tShow r <> ", _" <> name <> "@GOTPAGEOFF]"
                 addRegisterCache arg r
                 return r
      )
      return maybeInRegister

loadArgIntoReg :: Word -> Arg -> RegCodegen ()
loadArgIntoReg r arg = case arg of
                         DataOffset doff -> do
                             append $ "ADRP " <> "X" <> tShow r <> ", .dat@PAGE"
                             append $ "ADD " <> "X" <> tShow r <> ", X" <> tShow r <> ", .dat@PAGEOFF"
                             append $ "ADD " <> "X" <> tShow r <> ", X" <> tShow r <> ", #" <> tShow doff
                         Literal a -> let b1 = a .&. 0xFFFF
                                          b2 = shiftR a 16 .&. 0xFFFF
                                          b3 = shiftR a (16*2) .&. 0xFFFF
                                          b4 = shiftR a (16*3) .&. 0xFFFF
                                      in append $ "MOV X" <> tShow r <> ", #" <> tShow b1 <> "\n" <>
                                         (if b2 == 0 then "" else "MOVK X" <> tShow r <> ", #" <> tShow b2 <> ", LSL 16\n" <>
                                           if b3 == 0 then "" else "MOVK X" <> tShow r <> ", #" <> tShow b3 <> ", LSL 32\n" <>
                                           if b4 == 0 then "" else "MOVK X" <> tShow r <> ", #" <> tShow b4 <> ", LSL 48\n")
                         AutoVar autoVarOffset -> loadVarInStack r autoVarOffset
                         Deref autoVarOffset -> do
                             loadArgIntoReg 17 (AutoVar autoVarOffset)
                             loadVarInMem r 17
                         External name -> do
                             append $ "ADRP X" <> tShow r <> ", _" <> name <> "@GOTPAGE"
                             append $ "LDR X" <> tShow r <> ", [X" <> tShow r <> ", _" <> name <> "@GOTPAGEOFF]"
                             append $ "LDR X" <> tShow r <> ", [X" <> tShow r <> "]"
                         Ref offset -> do
                             append $ "MOV X" <> tShow r <> ", FP"
                             append $ "ADD X" <> tShow r <> ", X" <> tShow r <> ", #" <> tShow (offset*8)
                         RefExternal name -> do
                             append $ "ADRP X" <> tShow r <> ", _" <> name <> "@GOTPAGE"
                             append $ "LDR X" <> tShow r <> ", [X" <> tShow r <> ", _" <> name <> "@GOTPAGEOFF]"

aOp :: T.Text -> Int -> Int -> Op -> RegCodegen ()
aOp funName countParam countAutoVars o = case o of
          Funcall offset fnLoc fnArgs -> do
              saveRegisters
              zipWithM_ loadArgIntoReg [0..] fnArgs
              case fnLoc of
                (External s) -> do
                    c <- getState
                    let isVariadic = find (\(x, _) -> x == s) (aVariadics c)
                    maybe
                      ( do
                            append $ "BL _" <> s
                            addRegisterCache (AutoVar offset) 0
                      )
                      (\v -> do
                            let minArgs = snd v
                            let ss = alignStackOffset $ (length fnArgs - minArgs)*8
                            append $ "SUB SP, SP, #" <> tShow ss
                            traverse_
                              (\r -> append $ "STR X" <> tShow r <> ", [SP, " <> tShow ((r-minArgs)*8) <> "]" )
                              [minArgs..(length fnArgs - minArgs)]
                            append $ "BL _" <> s
                            addRegisterCache (AutoVar offset) 0
                            append $ "ADD SP, SP, #" <> tShow ss
                            return ()
                      )
                      isVariadic
                a -> do
                    loadArgIntoReg 16 a
                    append "BLR X16"
                    addRegisterCache (AutoVar offset) 0
          OpBin operator resultAutoVar lhs rhs -> aBinary operator resultAutoVar lhs rhs
          AutoAssign loc arg -> do
              argR <- aArg arg
              maybeInRegister <- findArgInRegisters (AutoVar loc)
              if isJust maybeInRegister
                then append $ "MOV X" <> tShow (fromJust maybeInRegister) <> ", X" <> tShow argR
                else do
                  r <- getEmptyRegister
                  append $ "MOV X" <> tShow r <> ", X" <> tShow argR
                  addRegisterCache (AutoVar loc) r
          MemoryAssign ptrLoc arg -> do
              argR <- aArg arg
              ptrR <- aArg (AutoVar ptrLoc)
              storeVarInMem argR ptrR
          ExternalAssign loc arg -> do
              argR <- aArg arg
              r <- getEmptyRegister
              append $ "ADRP X" <> tShow r <> ", _" <> loc <> "@GOTPAGE"
              append $ "LDR X" <> tShow r <> ", [X" <> tShow r <> ", _" <> loc <> "@GOTPAGEOFF]"
              append $ "STR X" <> tShow argR <> ", [X" <> tShow r <> ", #0]\n"
          Index dest ptsArg offsetArg -> do
              ptrR <- aArg ptsArg
              offsetR <- aArg offsetArg
              tempR <- getEmptyRegister
              append $ "MOV X" <> tShow tempR <> ", #8"
              append $ "MUL X" <> tShow tempR <> ", X" <> tShow offsetR <> ", X" <> tShow tempR
              append $ "ADD X" <> tShow tempR <> ", X" <> tShow ptrR <> ", X" <> tShow tempR
              maybeInRegister <- findArgInRegisters (AutoVar dest)
              if isJust maybeInRegister
                then append $ "MOV X" <> tShow (fromJust maybeInRegister) <> ", X" <> tShow tempR
                else addRegisterCache (AutoVar dest) tempR
          Label labelN -> do
              saveRegisters
              append $ funName <> tShow labelN <> ":"
          JmpLabel labelN -> do
              saveRegisters
              append $ "B " <> funName <> tShow labelN
          JmpIfZeroLabel labelN arg -> do
              condR <- aArg arg
              append $ "CMP X" <> tShow condR <> ", #0"
              saveRegisters
              append $ "B.EQ " <> funName <> tShow labelN
          Return Nothing -> aFunctionEpilogue countParam countAutoVars
          Return (Just arg) -> do
              saveRegisters
              loadArgIntoReg 0 arg
              aFunctionEpilogue countParam countAutoVars
          UnaryNot dest arg -> do
              r <- aArg arg
              append $ "CMP X" <> tShow r <> ", #0"
              append $ "CSET X" <> tShow r <> ", EQ"
              append $ "CSET X" <> tShow r <> ", EQ"
              addRegisterCache (AutoVar dest) r
          Negate dest arg -> do
              r <- aArg arg
              append $ "NEG X" <> tShow r <> ", X" <> tShow r
              addRegisterCache (AutoVar dest) r
          Asm a -> do
              saveRegisters
              traverse_ append a
          NoOp (UpdateStack size) -> do
              s <- registerStates <$> getState
              let b = filter (\(_,a,_) -> case a of
                                            AutoVar a -> a < size
                                            Deref a   -> a < size
                                            _         -> True
                             ) s
              updateState (\s -> s { registerStates = b })

aBinary :: BinOp -> Word -> Arg -> Arg -> RegCodegen ()
aBinary binOp loc lArg rArg = do
  argL <- aArg lArg
  argR <- aArg rArg
  maybeInRegister <- findArgInRegisters (AutoVar loc)
  r <- maybe
       (do r' <- getEmptyRegister
           addRegisterCache (AutoVar loc) r'
           return r')
       return maybeInRegister
  append $ case binOp of
              Add             -> "ADD X" <> tShow r <> ", X" <> tShow argL <> ", X" <> tShow argR <> "\n"
              Subtract        -> "SUB X" <> tShow r <> ", X" <> tShow argL <> ", X" <> tShow argR <> "\n"
              Multiply        -> "MUL X" <> tShow r <> ", X" <> tShow argL <> ", X" <> tShow argR <> "\n"
              Equal           -> "CMP X" <> tShow argL <> ", X" <> tShow argR <> "\n" <>
                                 "CSET X" <> tShow r <> ", EQ\n"
              NotEqual        -> "CMP X" <> tShow argL <> ", X" <> tShow argR <> "\n" <>
                                 "CSET X" <> tShow r <> ", NE\n"
              LessThan        -> "CMP X" <> tShow argL <> ", X" <> tShow argR <> "\n" <>
                                 "CSET X" <> tShow r <> ", LT\n"
              MoreThan        -> "CMP X" <> tShow argL <> ", X" <> tShow argR <> "\n" <>
                                 "CSET X" <> tShow r <> ", GT\n"
              LessThanOrEqual -> "CMP X" <> tShow argL <> ", X" <> tShow argR <> "\n" <>
                                 "CSET X" <> tShow r <> ", LE\n"
              MoreThanOrEqual -> "CMP X" <> tShow argL <> ", X" <> tShow argR <> "\n" <>
                                 "CSET X" <> tShow r <> ", GE\n"
              Modulo          -> "SDIV X" <> tShow r <> ", X" <> tShow argL <> ", X" <> tShow argR <> "\n" <>   -- suppose we are doing a%b. x2 holds a/b quotient
                                 "MSUB X" <> tShow r <> ", X" <> tShow r <> ", X" <> tShow argR <> ", X" <> tShow argL <> "\n"  -- which is q then we do (q*b -a) which is mod
              Or              -> "ORR X" <> tShow r <> ", X" <> tShow argL <> ", X" <> tShow argR <> "\n"
              Divide          -> "SDIV X" <> tShow r <> ", X" <> tShow argL <> ", X" <> tShow argR <> "\n"
