module TargetGasAArch64MacOS where

import Generator
import BParser (BBinary(Add, Subtract, Multiply, Equal, NotEqual))
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
              concatMap (\x->aOp (funName f) (paramsCount f) (autoVarCount f) x ++ "\n") (body f) ++ "\n" ++
              aFunctionEpilogue (paramsCount f) (fromIntegral $ autoVarCount f)

aFunctionPrologue :: String -> Word -> Word -> String
aFunctionPrologue name countParam countAutoVars = "_" ++ name ++ ":\n" ++
                                                  "STP LR, FP, [SP, #-16]!\n" ++
                                                  "SUB SP, SP, #" ++ show stackOffset ++ "\n" ++
                                                  "MOV FP, SP\n" ++
                                                  concat (zipWith storeVarOnStack [0..countParam] [0..countParam]) 
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

loadVarFromStack :: Word -> Word -> String
loadVarFromStack destReg offset = "LDR " ++ "X" ++ show destReg ++ ", [FP, #" ++ show (offset*8) ++ "]\n"

aOp :: String -> Word -> Word -> Op -> String
aOp funName countParam countAutoVars o = case o of
          Funcall offset fnLoc fnArgs -> concat (zipWith aArg [0..] fnArgs) ++ 
                                         fl fnLoc ++ "\n" ++
                                         storeVarOnStack 0 offset
          OpBin operator resultAutoVar lhs rhs -> aBinary operator resultAutoVar lhs rhs
          AutoAssign loc arg -> aArg 0 arg ++ storeVarOnStack 0 loc
          Label labelN -> funName ++ show labelN ++ ":"
          JmpLabel labelN -> "B " ++ funName ++ show labelN
          JmpIfZeroLabel labelN arg -> aArg 0 arg ++
                                      "CMP X0, #0\n" ++
                                      "B.EQ " ++ funName ++ show labelN
          Return Nothing -> aFunctionEpilogue countParam countAutoVars
          Return (Just arg) -> aArg 0 arg ++
                               aFunctionEpilogue countParam countAutoVars
    where fl (External s) = "BL _" ++ s
          fl a = aArg 16 a ++ "\n" ++ "BLR X16"

aArg :: Word -> Arg -> String
aArg reg arg = case arg of
             DataOffset doff -> "ADRP " ++ "X" ++ show reg ++ ", .dat@PAGE" ++ "\n" ++
                                "ADD " ++ "X" ++ show reg ++ ", X" ++ show reg ++ ", .dat@PAGEOFF\n" ++
                                "ADD " ++ "X" ++ show reg ++ ", X" ++ show reg ++ ", #" ++ show doff ++ "\n"
             Literal a -> "MOV X" ++ show reg ++ ", #" ++ show a ++ "\n"
             AutoVar autoVarOffset -> loadVarFromStack reg autoVarOffset

aBinary :: BinOp -> Word -> Arg -> Arg -> String
aBinary binOp resultLoc lArg rArg = aArg 1 lArg ++
                                    aArg 2 rArg ++
                                    (case binOp of
                                      Add      -> "ADD X0, X1, X2\n"
                                      Subtract -> "SUB X0, X1, X2\n"
                                      Multiply -> "MUL X0, X1, X2\n"
                                      Equal    -> "CMP X1, X2\n" ++
                                                  "CSET X0, EQ\n"
                                      NotEqual -> "CMP X1, X2\n" ++
                                                  "CSET X0, NE\n"
                                    ) ++
                                    storeVarOnStack 0 resultLoc
