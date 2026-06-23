module TargetGasAArch64MacOS where

import Generator
import BParser (BBinary(..))
import Data.Word
import Data.List
import Data.Maybe

asm :: IRProgram -> String
asm p = aProgramPrologue ++ "\n" ++
        concatMap aFunction (functions p) ++ "\n" ++
        aGlobalVarSection (globalVars p) ++ "\n" ++
        aDataSection (staticData p)

aProgramPrologue :: String
aProgramPrologue = ".text"

aDataSection :: [Word8] -> String
aDataSection a = ".data\n.dat: .byte " ++ intercalate "," (map show a)

aGlobalVarSection :: [(String, Maybe Int, [Arg])] -> String
aGlobalVarSection = concatMap (\(s, ms, args) ->
                                     if isNothing ms
                                     then aGlobalVar s args
                                     else aGlobalVector s (fromJust ms) args
                                )

aGlobalVar :: String -> [Arg] -> String
aGlobalVar vName initData = ".data\n" ++
                            ".global _" ++ vName ++ "\n" ++
                            ".p2align 3 // investigate why this is needed\n"++
                            "_" ++ vName ++ ":\n" ++
                            if null initData then ".quad 0"
                            else concatMap (\a -> ".quad " ++ aGlobalVarArg a ++ "\n") initData

aGlobalVector :: String -> Int -> [Arg] -> String
aGlobalVector vName vSize initData = undefined

aGlobalVarArg :: Arg -> String
aGlobalVarArg (External a)   = "_" ++ a
aGlobalVarArg (Literal a)    = show a
aGlobalVarArg (DataOffset a) = ".dat +" ++ show a
                                     ;
aFunction :: Function -> String
aFunction f = aFunctionPrologue (funName f) (paramsCount f) (autoVarCount f) ++ "\n" ++
              concatMap (\x->aOp (funName f) (paramsCount f) (autoVarCount f) x ++ "\n") (body f) ++ "\n" ++
              aFunctionEpilogue (paramsCount f) (fromIntegral $ autoVarCount f)

aFunctionPrologue :: String -> Word -> Word -> String
aFunctionPrologue name countParam countAutoVars = "\n.global _" ++ name ++ "\n" ++
                                                  ".p2align 4\n" ++
                                                  "_" ++ name ++ ":\n" ++
                                                  "STP LR, FP, [SP, #-16]!\n" ++
                                                  "SUB SP, SP, #" ++ show stackOffset ++ "\n" ++
                                                  "MOV FP, SP\n" ++
                                                  if countParam==0 then []
                                                  else concat (zipWith storeVarOnStack [0..(countParam - 1)] [0..(countParam - 1)]) 
    where stackOffset = if mod ccc 16 == 0 then ccc else div ccc 16*16 + 16
          ccc = (countParam + countAutoVars)*8

aFunctionEpilogue :: Word -> Word -> String
aFunctionEpilogue countParam countAutoVars = "ADD SP, SP, #" ++ show stackOffset ++ "\n" ++
                                             "LDP LR, FP, [SP], #16\n" ++
                                             "RET\n"
    where stackOffset = if mod ccc 16 == 0 then ccc else div ccc 16*16 + 16
          ccc = (countParam + countAutoVars)*8

storeVarOnStack :: Word -> Word -> String
storeVarOnStack reg offset = "STR " ++ "X" ++ show reg ++ ", [FP, #" ++ show (offset*8) ++ "]\n"

loadVarInStack :: Word -> Word -> String
loadVarInStack destReg offset = "LDR " ++ "X" ++ show destReg ++ ", [FP, #" ++ show (offset*8) ++ "]\n"

storeVarInMem :: Word -> Word -> String
storeVarInMem reg ptrOffset = loadVarInStack (reg+1) ptrOffset ++
                              "STR X" ++ show reg ++ ", [X" ++ show (reg+1) ++ ", #0]"
                              ++ "\n; storing variable in memory"

loadVarInMem :: Word -> Word -> String
loadVarInMem destReg ptrOffset = loadVarInStack destReg ptrOffset ++
                                 "LDR X0, [X" ++ show destReg ++ ", #0]\n"
                                 ++ "\n; loading variable in memory\n"

aOp :: String -> Word -> Word -> Op -> String
aOp funName countParam countAutoVars o = case o of
          Funcall offset fnLoc fnArgs -> concat (zipWith aArg [0..] fnArgs) ++ 
                                         fl fnLoc ++ "\n" ++
                                         storeVarOnStack 0 offset
          OpBin operator resultAutoVar lhs rhs -> aBinary operator resultAutoVar lhs rhs
          AutoAssign loc arg -> aArg 0 arg ++ storeVarOnStack 0 loc
          MemoryAssign ptrLoc arg -> aArg 0 arg ++ storeVarInMem 0 ptrLoc
          ExternalAssign loc arg -> aArg 0 arg ++
                                    "ADRP X1, _" ++ loc ++ "@GOTPAGE\n" ++
                                    "LDR X1, [X1, _" ++ loc ++ "@GOTPAGEOFF]\n" ++
                                    "STR X0, [X1, #0]\n"
          Index dest ptsArg offsetArg -> aArg 1 ptsArg ++ aArg 2 offsetArg ++
                                         "MOV X3, #8\n" ++
                                         "MUL X2, X2, X3\n" ++
                                         "ADD X0, X1, X2\n" ++
                                         storeVarOnStack 0 dest
          Label labelN -> funName ++ show labelN ++ ":"
          JmpLabel labelN -> "B " ++ funName ++ show labelN
          JmpIfZeroLabel labelN arg -> aArg 0 arg ++ 
                                      "CMP X0, #0\n" ++
                                      "B.EQ " ++ funName ++ show labelN
          Return Nothing -> aFunctionEpilogue countParam countAutoVars
          Return (Just arg) -> aArg 0 arg ++
                               aFunctionEpilogue countParam countAutoVars
          UnaryNot dest arg -> aArg 0 arg ++
                               "CMP X0, #0\n" ++
                               "CSET X0, EQ\n" ++
                               storeVarOnStack 0 dest
          Negate dest arg -> aArg 0 arg ++
                             "NEG X0, X0\n" ++
                             storeVarOnStack 0 dest
          Asm a -> unlines a
    where fl (External s) = "BL _" ++ s
          fl a = aArg 16 a ++ "\n" ++ "BLR X16"

aArg :: Word -> Arg -> String
aArg reg arg = case arg of
             DataOffset doff -> "ADRP " ++ "X" ++ show reg ++ ", .dat@PAGE" ++ "\n" ++
                                "ADD " ++ "X" ++ show reg ++ ", X" ++ show reg ++ ", .dat@PAGEOFF\n" ++
                                "ADD " ++ "X" ++ show reg ++ ", X" ++ show reg ++ ", #" ++ show doff ++ "\n"
             Literal a -> "MOV X" ++ show reg ++ ", #" ++ show a ++ "\n"
             AutoVar autoVarOffset -> loadVarInStack reg autoVarOffset
             Deref autoVarOffset -> loadVarInMem reg autoVarOffset
             External name -> "ADRP X" ++ show reg ++ ", _" ++ name ++ "@GOTPAGE\n" ++
                              "LDR X" ++ show reg ++ ", [X" ++ show reg ++ ", _" ++ name ++ "@GOTPAGEOFF]\n" ++
                              "LDR X" ++ show reg ++ ", [X" ++ show reg ++ "]\n"

aBinary :: BinOp -> Word -> Arg -> Arg -> String
aBinary binOp resultLoc lArg rArg = aArg 1 lArg ++
                                    aArg 2 rArg ++
                                    (case binOp of
                                      Add             -> "ADD X0, X1, X2\n"
                                      Subtract        -> "SUB X0, X1, X2\n"
                                      Multiply        -> "MUL X0, X1, X2\n"
                                      Equal           -> "CMP X1, X2\n" ++
                                                         "CSET X0, EQ\n"
                                      NotEqual        -> "CMP X1, X2\n" ++
                                                         "CSET X0, NE\n"
                                      LessThan        -> "CMP X1, X2\n" ++
                                                         "CSET X0, LT\n"
                                      MoreThan        -> "CMP X1, X2\n" ++
                                                         "CSET X0, GT\n"
                                      LessThanOrEqual -> "CMP X1, X2\n" ++
                                                         "CSET X0, LE\n"
                                      MoreThanOrEqual -> "CMP X1, X2\n" ++
                                                         "CSET X0, GE\n"
                                      Modulo          -> "SDIV X0, X1, X2\n" ++   -- suppose we are doing a%b. x2 holds a/b quotient
                                                         "MSUB X0, X0, X2, X1\n"  -- which is q then we do (q*b -a) which is mod
                                      Or              -> "ORR X0, X1, X2\n"
                                      Divide          -> "SDIV X0, X1, X2\n" --
                                    ) ++
                                    storeVarOnStack 0 resultLoc
