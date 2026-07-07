module Codegen.GasAArch64 where

import Codegen.Common
import Generator

import BParser        (BBinary (..))
import Control.Monad
import Data.Bits
import Data.List
import Data.Maybe
import Data.Word

data State = State {
    asmOutput           :: [String],
    count               :: Word,
    registerStates      :: [(Word, Arg, Word)], -- Register, arg, age
    firstTouchToAutoVar :: [Int], -- list of auto vars not yet initialized
    codegenLog :: [String]
    } deriving (Show)

type RegCodegen = StateM State

addCount :: RegCodegen ()
addCount = StateM $ \s -> (s { count = count s + 1, codegenLog = codegenLog s ++
                               ("Count: " ++ show (count s) ++ "\t" ++
                                (let a = show (registerStates s) in "Reg " ++ a ++ (replicate (135 - length a) ' ')) ++ "\t" ++
                                (let a = asmOutput s in if null a then "" else last a)):[]},())

setState :: State -> RegCodegen ()
setState s = StateM (const (s,())) >>= const addCount

getState :: RegCodegen State
getState = addCount >>= const (StateM $ \s -> (s,s))

updateState :: (State -> State) -> RegCodegen ()
updateState f = addCount >>= const (StateM $ \s -> (f s,()))

append :: String -> RegCodegen ()
append ins = updateState $ \s -> s { asmOutput = asmOutput s ++ [ins] }

gasAArch64 = Target "gasAArch64" False asm

asm :: IRProgram -> IO String
asm p = do
    let s = fst $ runStateM (generateAsm p) (State [] 0 [] [] [])
    if False
      then do
        print $ count s
        print $ registerStates s
        print $ firstTouchToAutoVar s
        putStrLn "---"
        putStr $ unlines $ codegenLog s
        putStrLn "---"
      else return ()
    return $ unlines $ asmOutput $ s

generateAsm :: IRProgram -> RegCodegen ()
generateAsm p = do
    aProgramPrologue
    traverse aFunction (functions p)
    aGlobalVarSection (globalVars p)
    traverse aNakedFunctionSection (nakedFunctions p)
    aDataSection (staticData p)

aProgramPrologue :: RegCodegen ()
aProgramPrologue = do
    append ".text"

aGlobalVarSection :: [(String, Maybe Int, [Arg])] -> RegCodegen ()
aGlobalVarSection l = do
    traverse (\(s, ms, args) ->
                if isNothing ms
                then aGlobalVar s args
                else aGlobalVector s (fromJust ms) args
             ) l
    return ()

aGlobalVar :: String -> [Arg] -> RegCodegen ()
aGlobalVar vName initData = do
    append $ ".data"
    append $ ".global _" ++ vName
    append $ ".p2align 3 // investigate why this is needed"
    append $ "_" ++ vName ++ ":"
    let as = if null initData
             then [".quad 0"]
             else map (\a -> ".quad " ++ aGlobalVarArg a) initData
    updateState $ \s -> s { asmOutput = asmOutput s ++ as }

aGlobalVector :: String -> Int -> [Arg] -> RegCodegen ()
aGlobalVector vName vSize initData = undefined

aDataSection :: [Word8] -> RegCodegen ()
aDataSection a = do
    append ".data"
    append $ ".dat: .byte " ++ intercalate "," (map show a)

aGlobalVarArg :: Arg -> String
aGlobalVarArg (External a)   = "_" ++ a
aGlobalVarArg (Literal a)    = show a
aGlobalVarArg (DataOffset a) = ".dat +" ++ show a

aNakedFunctionSection :: NFunction -> RegCodegen ()
aNakedFunctionSection (NFunction nfName nfLoc nfBlock) = do
    append $  ".global _" ++ nfName
    append $  ".p2align 4"
    append $ "_" ++ nfName ++ ":"
    append $  unlines nfBlock

storeVarOnStack :: Int -> Int -> RegCodegen ()
storeVarOnStack reg offset = append $ "STR X" ++ show reg ++ ", [FP, #" ++ show (offset*8) ++ "]"

loadVarInStack :: Word -> Word -> RegCodegen ()
loadVarInStack destReg offset = append $ "LDR X" ++ show destReg ++ ", [FP, #" ++ show (offset*8) ++ "]"

storeVarInMem :: Word -> Word -> RegCodegen ()
storeVarInMem reg ptrReg = do
    append $ "STR X" ++ show reg ++ ", [X" ++ show ptrReg ++ ", #0]"
    append "; storing variable in memory"

loadVarInMem :: Word -> Word -> RegCodegen ()
loadVarInMem destReg ptrReg = do
    append $ "LDR X" ++ show destReg ++ ", [X" ++ show ptrReg ++ ", #0]"
    append "; loading variable in memory"

saveRegisters :: RegCodegen ()
saveRegisters = do
    s <- getState
    traverse saveOneRegister (registerStates s)
    updateState $ \s -> s { registerStates = [] }

saveOneRegister :: (Word, Arg, Word) -> RegCodegen ()
saveOneRegister (register, arg, age) = case arg of
                                         AutoVar offset -> storeVarOnStack (fromIntegral register) (fromIntegral offset)
                                         _ -> return ()

findArgInRegisters :: Arg -> RegCodegen (Maybe Word)
findArgInRegisters arg = do
    s <- getState
    return $ fmap (\(a,_,_) -> a) $ find (\(_,a,_) -> a==arg) (registerStates s)

getEmptyRegister :: RegCodegen Word
getEmptyRegister = do
    s <- getState
    let l = length (registerStates s)
    if l >= 16
      then do
        undefined
      else return $ fromIntegral l

addRegisterCache :: Arg -> Word -> RegCodegen ()
addRegisterCache arg register = updateState $ \s -> s { registerStates = (register, arg, (count s)):(registerStates s) }

isVarTouched :: Word -> RegCodegen Bool
isVarTouched autoVar = do
    s <- getState
    let untouched = elem (fromIntegral autoVar) (firstTouchToAutoVar s)
    if untouched
      then do
        updateState $ \s -> s { firstTouchToAutoVar = delete (fromIntegral autoVar) (firstTouchToAutoVar s) }
        return False
      else return True

aFunction :: Function -> RegCodegen ()
aFunction f = do
    aFunctionPrologue (funName f) (paramsCount f) (autoVarCount f)
    updateState $ \s -> s { registerStates = [], firstTouchToAutoVar = [(paramsCount f)..(paramsCount f + autoVarCount f - 1)] }
    traverse (aOp (funName f) (paramsCount f) (autoVarCount f)) (body f)
    aFunctionEpilogue (paramsCount f) (autoVarCount f)
    append ""
    return ()

aFunctionPrologue :: String -> Int -> Int -> RegCodegen ()
aFunctionPrologue name countParam countAutoVars = do
    append $ ".global _" ++ name
    append $ ".p2align 4"
    append $ "_" ++ name ++ ":"
    append $ "STP LR, FP, [SP, #-16]!"
    append $ "SUB SP, SP, #" ++ show stackOffset
    append $ "MOV FP, SP"
    if countParam==0
      then return ()
      else zipWithM_ storeVarOnStack [0..(countParam - 1)] [0..(countParam - 1)]
      where stackOffset = if mod ccc 16 == 0 then ccc else div ccc 16*16 + 16
            ccc = (countParam + countAutoVars)*8

aFunctionEpilogue :: Int -> Int -> RegCodegen ()
aFunctionEpilogue countParam countAutoVars = do
    append $ "ADD SP, SP, #" ++ show stackOffset
    append $ "LDP LR, FP, [SP], #16"
    append $ "RET"
      where stackOffset = if mod ccc 16 == 0 then ccc else div ccc 16*16 + 16
            ccc = (countParam + countAutoVars)*8

aArg :: Arg -> RegCodegen Word
aArg arg = do
    maybeInRegister <- findArgInRegisters arg
    if isJust maybeInRegister
      then return $ fromJust maybeInRegister
      else case arg of
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
             -- External name -> "ADRP X" ++ show reg ++ ", _" ++ name ++ "@GOTPAGE\n" ++
                              -- "LDR X" ++ show reg ++ ", [X" ++ show reg ++ ", _" ++ name ++ "@GOTPAGEOFF]\n" ++
                              -- "LDR X" ++ show reg ++ ", [X" ++ show reg ++ "]\n"

loadArgIntoReg :: Word -> Arg -> RegCodegen ()
loadArgIntoReg r arg = case arg of
                         DataOffset doff -> do
                             append $ "ADRP " ++ "X" ++ show r ++ ", .dat@PAGE"
                             append $ "ADD " ++ "X" ++ show r ++ ", X" ++ show r ++ ", .dat@PAGEOFF"
                             append $ "ADD " ++ "X" ++ show r ++ ", X" ++ show r ++ ", #" ++ show doff
                         Literal a -> let b1 = a .&. 0xFFFF
                                          b2 = shiftR a 16 .&. 0xFFFF
                                          b3 = shiftR a (16*2) .&. 0xFFFF
                                          b4 = shiftR a (16*3) .&. 0xFFFF
                                      in append $ "MOV X" ++ show r ++ ", #" ++ show b1 ++ "\n" ++
                                         (if b2 == 0 then "" else "MOVK X" ++ show r ++ ", #" ++ show b2 ++ ", LSL 16\n" ++
                                           if b3 == 0 then "" else "MOVK X" ++ show r ++ ", #" ++ show b3 ++ ", LSL 32\n" ++
                                           if b4 == 0 then "" else "MOVK X" ++ show r ++ ", #" ++ show b4 ++ ", LSL 48\n")
                         AutoVar autoVarOffset -> loadVarInStack r autoVarOffset
                         Deref autoVarOffset -> do
                             ptrR <- aArg (AutoVar autoVarOffset)
                             r <- getEmptyRegister
                             loadVarInMem r ptrR
                                         
aOp :: String -> Int -> Int -> Op -> RegCodegen ()
aOp funName countParam countAutoVars o = case o of
          Funcall offset fnLoc fnArgs -> do
              saveRegisters
              zipWithM_ loadArgIntoReg [0..] fnArgs
              fl fnLoc
              addRegisterCache (AutoVar offset) 0
          OpBin operator resultAutoVar lhs rhs -> aBinary operator resultAutoVar lhs rhs
          AutoAssign loc arg -> do
              argR <- aArg arg
              maybeInRegister <- findArgInRegisters (AutoVar loc)
              if isJust maybeInRegister
                then append $ "MOV X" ++ show (fromJust maybeInRegister) ++ ", X" ++ show argR
                else do
                  r <- getEmptyRegister
                  append $ "MOV X" ++ show r ++ ", X" ++ show argR
                  addRegisterCache (AutoVar loc) r
          MemoryAssign ptrLoc arg -> do
              argR <- aArg arg
              storeVarInMem 0 ptrLoc
          ExternalAssign loc arg -> do
              argR <- aArg arg
              r <- getEmptyRegister
              append $ "ADRP X" ++ show r ++ ", _" ++ loc ++ "@GOTPAGE"
              append $ "LDR X" ++ show r ++ ", [X" ++ show r ++ ", _" ++ loc ++ "@GOTPAGEOFF]"
              append $ "STR X" ++ show argR ++ ", [X" ++ show r ++ ", #0]\n"
          Index dest ptsArg offsetArg -> do
              ptrR <- aArg ptsArg
              offsetR <- aArg offsetArg
              tempR <- getEmptyRegister
              append $ "MOV X" ++ show tempR ++ ", #8"
              append $ "MUL X" ++ show tempR ++ ", X" ++ show offsetR ++ ", X" ++ show tempR
              append $ "ADD X" ++ show tempR ++ ", X" ++ show ptrR ++ ", X" ++ show tempR

              maybeInRegister <- findArgInRegisters (AutoVar dest)
              if isJust maybeInRegister
                then append $ "MOV X" ++ show (fromJust maybeInRegister) ++ ", X" ++ show tempR
                else addRegisterCache (AutoVar dest) tempR

          -- Index dest ptsArg offsetArg -> aArg 1 ptsArg ++ aArg 2 offsetArg ++
          --                                "MOV X3, #8\n" ++
          --                                "MUL X2, X2, X3\n" ++
          --                                "ADD X0, X1, X2\n" ++
          --                                storeVarOnStack 0 dest
          Label labelN -> do
              saveRegisters
              append $ funName ++ show labelN ++ ":"
          JmpLabel labelN -> do
              saveRegisters
              append $ "B " ++ funName ++ show labelN
          JmpIfZeroLabel labelN arg -> do
              condR <- aArg arg
              append $ "CMP X" ++ show condR ++ ", #0"
              saveRegisters
              append $ "B.EQ " ++ funName ++ show labelN
          Return Nothing -> aFunctionEpilogue countParam countAutoVars
          Return (Just arg) -> do
              loadArgIntoReg 0 arg
              aFunctionEpilogue countParam countAutoVars
          -- UnaryNot dest arg -> aArg 0 arg ++
          --                      "CMP X0, #0\n" ++
          --                      "CSET X0, EQ\n" ++
          --                      "CSET X0, EQ\n" ++
          --                      storeVarOnStack 0 dest
          -- Negate dest arg -> aArg 0 arg ++
          --                    "NEG X0, X0\n" ++
          --                    storeVarOnStack 0 dest
          Asm a -> do
              saveRegisters
              traverse append a
              return ()
  where fl (External s) = append $ "BL _" ++ s
        fl a = do
            loadArgIntoReg 16 a
            append "BLR X16"

aBinary :: BinOp -> Word -> Arg -> Arg -> RegCodegen ()
aBinary binOp loc lArg rArg = do
  argL <- aArg lArg
  argR <- aArg rArg
  r <- getEmptyRegister
  append $ case binOp of
              Add             -> "ADD X" ++ show r ++ ", X" ++ show argL ++ ", X" ++ show argR ++ "\n"
              Subtract        -> "SUB X" ++ show r ++ ", X" ++ show argL ++ ", X" ++ show argR ++ "\n"
              Multiply        -> "MUL X" ++ show r ++ ", X" ++ show argL ++ ", X" ++ show argR ++ "\n"
              Equal           -> "CMP X" ++ show argL ++ ", X" ++ show argR ++ "\n" ++
                                 "CSET X" ++ show r ++ ", EQ\n"
              NotEqual        -> "CMP X" ++ show argL ++ ", X" ++ show argR ++ "\n" ++
                                 "CSET X" ++ show r ++ ", NE\n"
              LessThan        -> "CMP X" ++ show argL ++ ", X" ++ show argR ++ "\n" ++
                                 "CSET X" ++ show r ++ ", LT\n"
              MoreThan        -> "CMP X" ++ show argL ++ ", X" ++ show argR ++ "\n" ++
                                 "CSET X" ++ show r ++ ", GT\n"
              LessThanOrEqual -> "CMP X" ++ show argL ++ ", X" ++ show argR ++ "\n" ++
                                 "CSET X" ++ show r ++ ", LE\n"
              MoreThanOrEqual -> "CMP X" ++ show argL ++ ", X" ++ show argR ++ "\n" ++
                                 "CSET X" ++ show r ++ ", GE\n"
              Modulo          -> "SDIV X" ++ show r ++ ", X" ++ show argL ++ ", X" ++ show argR ++ "\n" ++   -- suppose we are doing a%b. x2 holds a/b quotient
                                 "MSUB X" ++ show r ++ ", X" ++ show r ++ ", X" ++ show argR ++ ", X" ++ show argL ++ "\n"  -- which is q then we do (q*b -a) which is mod
              Or              -> "ORR X" ++ show r ++ ", X" ++ show argL ++ ", X" ++ show argR ++ "\n"
              Divide          -> "SDIV X" ++ show r ++ ", X" ++ show argL ++ ", X" ++ show argR ++ "\n" --

  maybeInRegister <- findArgInRegisters (AutoVar loc)
  if isJust maybeInRegister
    then append $ "MOV X" ++ show (fromJust maybeInRegister) ++ ", X" ++ show r
    else do
      r <- getEmptyRegister
      append $ "MOV X" ++ show r ++ ", X" ++ show r
      addRegisterCache (AutoVar loc) r
