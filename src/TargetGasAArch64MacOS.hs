module TargetGasAArch64MacOS where

import Generator
import Data.Word
import Data.List

asm :: IRProgram -> String
asm p = aProgramPrologue ++ "\n" ++
        concatMap aFunction (functions p) ++ "\n" ++
        aDataSection (staticData p)

aProgramPrologue :: String
aProgramPrologue = ".text\n" ++
                   ".global _main\n" ++
                   ".align 4\n"

aDataSection :: [Word8] -> String
aDataSection a = ".data\n.dat: .byte " ++ intercalate "," (map show a)

aFunction :: Function -> String
aFunction f = aFunctionPrologue (funName f) (paramsCount f) (autoVarCount f) ++ "\n" ++
              concatMap aOp (body f) ++ "\n" ++
              aFunctionEpilogue (paramsCount f) (fromIntegral $ autoVarCount f)

aFunctionPrologue :: String -> Word -> Word -> String
aFunctionPrologue name countParam countAutoVars = "_" ++ name ++ ":\n" ++
                                                  "STP LR, FP, [SP, #-16]!\n" ++
                                                  "SUB SP, SP, #" ++ show stackOffset ++ "\n" ++
                                                  "MOV FP, SP\n" ++
                                                  concat (zipWith storeVarOnStack [0..countParam] [0..countParam]) 
    where stackOffset = if mod ccc 16 == 0 then ccc else div ccc 16*16 + 16
          ccc = (countParam + countAutoVars)*4

aFunctionEpilogue :: Word -> Word -> String
aFunctionEpilogue countParam countAutoVars = "ADD SP, SP, #" ++ show stackOffset ++ "\n" ++
                                                  "LDP LR, FP, [SP], #16\n"
    where stackOffset = if mod ccc 16 == 0 then ccc else div ccc 16*16 + 16
          ccc = (countParam + countAutoVars)*4

storeVarOnStack :: Word -> Word -> String
storeVarOnStack reg offset = "STR " ++ "X" ++ show reg ++ ", [FP, #" ++ show offset ++ "]\n"

loadVarFromStack :: Word -> Word -> String
loadVarFromStack destReg offset = "LDR " ++ "X" ++ show destReg ++ ", [FP, #" ++ show offset ++ "]\n"

aOp :: Op -> String
aOp o = case o of
          Funcall offset fnLoc fnArgs -> concat (zipWith aArg [0..] fnArgs) ++ 
                                         fl fnLoc ++ "\n" ++
                                         storeVarOnStack 0 offset
    where fl (External s) = "BL _" ++ s
          fl a = aArg 16 a ++ "\n" ++ "BLR X16"

aArg :: Word -> Arg -> String
aArg reg arg = case arg of
             DataOffset doff -> "ADRP " ++ "X" ++ show reg ++ ", .dat@PAGE" ++ "\n" ++
                                "ADD " ++ "X" ++ show reg ++ ", X" ++ show reg ++ ", .dat@PAGEOFF\n" ++
                                "ADD " ++ "X" ++ show reg ++ ", X" ++ show reg ++ ", #" ++ show doff ++ "\n"
             Literal a -> "MOV X" ++ show reg ++ ", #" ++ show a ++ "\n"
