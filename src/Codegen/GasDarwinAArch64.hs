module Codegen.GasDarwinAArch64 where
import Codegen.Common

import BParser        (BBinary (..))
import Data.Bits
import Data.List
import Data.Maybe
import Data.Word
import Generator
import System.Process
import Data.Foldable

gasDarwinAArch64 = Target
  "gasDarwinAArch64"
  buildRecipe
  
buildRecipe nC outputFileName sourceFiles objectFiles linkerFlags = do 
    traverse_ (\fileName -> do
               runIfChanged nC [fileName]
                 (getFileName ".s" fileName)
                 (compileFile (return . asm) False fileName)
               runIfChanged nC [getFileName ".s" fileName]
                 (getFileName ".o" fileName)
                 (prettyProcess $ readProcessWithExitCode "as"
                   ["-arch", "arm64", "-o", getFileName ".o" fileName, getFileName ".s" fileName] "")
             ) sourceFiles

    runIfChanged nC (objectFiles ++ map (getFileName ".o") sourceFiles)
       (takeWhile (/='.') outputFileName)
       (prettyProcess $ readProcessWithExitCode "gcc" (["-o", takeWhile (/='.') outputFileName] ++ map (getFileName ".o") sourceFiles ++ objectFiles ++ linkerFlags) "")
    return ()

asm :: IRProgram -> String
asm p = aProgramPrologue ++ "\n" ++
        concatMap (aFunction (variadics p)) (functions p) ++ "\n" ++
        aGlobalVarSection (globalVars p) ++ "\n" ++
        concatMap aNakedFunctionSection (nakedFunctions p) ++ "\n" ++
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

aNakedFunctionSection :: NFunction -> String
aNakedFunctionSection (NFunction nfName nfLoc nfBlock) = ".global _" ++ nfName ++ "\n" ++
                                                         ".p2align 4\n" ++
                                                         "_" ++ nfName ++ ":\n" ++
                                                         unlines nfBlock

aFunction :: [(String, Int)] -> Function -> String
aFunction vs f = aFunctionPrologue (funName f) (paramsCount f) (autoVarCount f) ++ "\n" ++
                 concatMap (\x->aOp vs (funName f) (paramsCount f) (autoVarCount f) x ++ "\n") (body f) ++ "\n" ++
                 aFunctionEpilogue (paramsCount f) (fromIntegral $ autoVarCount f)

aFunctionPrologue :: String -> Int -> Int -> String
aFunctionPrologue name countParam countAutoVars = "\n.global _" ++ name ++ "\n" ++
                                                  ".p2align 4\n" ++
                                                  "_" ++ name ++ ":\n" ++
                                                  "STP LR, FP, [SP, #-16]!\n" ++
                                                  "SUB SP, SP, #" ++ show stackOffset ++ "\n" ++
                                                  "MOV FP, SP\n" ++
                                                  concat (zipWith storeVarOnStack [0..(countParam - 1)] [0..(countParam - 1)])
    where stackOffset = if mod ccc 16 == 0 then ccc else div ccc 16*16 + 16
          ccc = (countParam + countAutoVars)*8
  
alignStackOffset ccc = if mod ccc 16 == 0 then ccc else div ccc 16*16 + 16

aFunctionEpilogue :: Int -> Int -> String
aFunctionEpilogue countParam countAutoVars = "ADD SP, SP, #" ++ show stackOffset ++ "\n" ++
                                             "LDP LR, FP, [SP], #16\n" ++
                                             "RET\n"
    where stackOffset = if mod ccc 16 == 0 then ccc else div ccc 16*16 + 16
          ccc = (countParam + countAutoVars)*8

storeVarOnStack :: Int -> Int -> String
storeVarOnStack reg offset = "STR " ++ "X" ++ show reg ++ ", [FP, #" ++ show (offset*8) ++ "]\n"

loadVarInStack :: Int -> Int -> String
loadVarInStack destReg offset = "LDR " ++ "X" ++ show destReg ++ ", [FP, #" ++ show (offset*8) ++ "]\n"

storeVarInMem :: Int -> Int -> String
storeVarInMem reg ptrOffset = loadVarInStack (reg+1) ptrOffset ++
                              "STR X" ++ show reg ++ ", [X" ++ show (reg+1) ++ ", #0]"
                              ++ "\n; storing variable in memory"

loadVarInMem :: Int -> Int -> String
loadVarInMem destReg ptrOffset = loadVarInStack destReg ptrOffset ++
                                 "LDR X" ++ show destReg ++ ", [X" ++ show destReg ++ ", #0]\n"
                                 ++ "\n; loading variable in memory\n"

aOp :: [(String, Int)] -> String -> Int -> Int -> Op -> String
aOp variadics funName countParam countAutoVars o = case o of
          Funcall offset fnLoc fnArgs -> concat (zipWith aArg [0..] fnArgs) ++
                                         case fnLoc of
                                           (External s) -> let isVariadic = find (\(x, _) -> x == s) variadics
                                                           in maybe ( "BL _" ++ s ++ "\n" ++
                                                                      storeVarOnStack 0 (fromIntegral offset))
                                                              ( \v -> let minArgs = snd v
                                                                          ss = alignStackOffset $ (length fnArgs - minArgs)*8
                                                                      in "SUB SP, SP, #" ++ show ss ++ "\n" ++
                                                                         concatMap (\r -> "STR X" ++ show r ++ ", [SP, " ++ show ((r-minArgs)*8) ++ "]\n")
                                                                         [minArgs..(length fnArgs - minArgs)] ++ "\n" ++
                                                                         "BL _" ++ s ++ "\n" ++
                                                                         storeVarOnStack 0 (fromIntegral offset) ++
                                                                         "ADD SP, SP, #" ++ show ss ++ "\n"
                                                              )
                                                              isVariadic
                                           a -> aArg 16 a ++ "\n" ++ "BLR X16\n" ++
                                                storeVarOnStack 0 (fromIntegral offset)
          OpBin operator resultAutoVar lhs rhs -> aBinary operator resultAutoVar lhs rhs
          AutoAssign loc arg -> aArg 0 arg ++ storeVarOnStack 0 (fromIntegral loc)
          MemoryAssign ptrLoc arg -> aArg 0 arg ++ storeVarInMem 0 (fromIntegral ptrLoc)
          ExternalAssign loc arg -> aArg 0 arg ++
                                    "ADRP X1, _" ++ loc ++ "@GOTPAGE\n" ++
                                    "LDR X1, [X1, _" ++ loc ++ "@GOTPAGEOFF]\n" ++
                                    "STR X0, [X1, #0]\n"
          Index dest ptsArg offsetArg -> aArg 1 ptsArg ++ aArg 2 offsetArg ++
                                         "MOV X3, #8\n" ++
                                         "MUL X2, X2, X3\n" ++
                                         "ADD X0, X1, X2\n" ++
                                         storeVarOnStack 0 (fromIntegral dest)
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
                               storeVarOnStack 0 (fromIntegral dest)
          Negate dest arg -> aArg 0 arg ++
                             "NEG X0, X0\n" ++
                             storeVarOnStack 0 (fromIntegral dest)
          Asm a -> unlines a
          NoOp _ -> ""

aArg :: Word -> Arg -> String
aArg reg arg = case arg of
             DataOffset doff -> "ADRP " ++ "X" ++ show reg ++ ", .dat@PAGE" ++ "\n" ++
                                "ADD " ++ "X" ++ show reg ++ ", X" ++ show reg ++ ", .dat@PAGEOFF\n" ++
                                "ADD " ++ "X" ++ show reg ++ ", X" ++ show reg ++ ", #" ++ show doff ++ "\n"
             Literal a -> let b1 = a .&. 0xFFFF
                              b2 = shiftR a 16 .&. 0xFFFF
                              b3 = shiftR a (16*2) .&. 0xFFFF
                              b4 = shiftR a (16*3) .&. 0xFFFF
                          in "MOV X" ++ show reg ++ ", #" ++ show b1 ++ "\n" ++
                             (if b2 == 0 then "" else "MOVK X" ++ show reg ++ ", #" ++ show b2 ++ ", LSL 16\n" ++
                             if b3 == 0 then "" else "MOVK X" ++ show reg ++ ", #" ++ show b3 ++ ", LSL 32\n" ++
                             if b4 == 0 then "" else "MOVK X" ++ show reg ++ ", #" ++ show b4 ++ ", LSL 48\n")

             AutoVar autoVarOffset -> loadVarInStack (fromIntegral reg) (fromIntegral autoVarOffset)
             Deref autoVarOffset -> loadVarInMem (fromIntegral reg) (fromIntegral autoVarOffset)
             External name -> "ADRP X" ++ show reg ++ ", _" ++ name ++ "@GOTPAGE\n" ++
                              "LDR X" ++ show reg ++ ", [X" ++ show reg ++ ", _" ++ name ++ "@GOTPAGEOFF]\n" ++
                              "LDR X" ++ show reg ++ ", [X" ++ show reg ++ "]\n"
             Ref offset -> "MOV X" ++ show reg ++ ", FP\n" ++
                           "ADD X" ++ show reg ++ ", X" ++ show reg ++ ", #" ++ show (offset*8) ++ "\n"
             RefExternal name -> "ADRP X" ++ show reg ++ ", _" ++ name ++ "@GOTPAGE\n" ++
                                 "LDR X" ++ show reg ++ ", [X" ++ show reg ++ ", _" ++ name ++ "@GOTPAGEOFF]\n"

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
                                      Divide          -> "SDIV X0, X1, X2\n"
                                    ) ++
                                    storeVarOnStack 0 (fromIntegral resultLoc)
