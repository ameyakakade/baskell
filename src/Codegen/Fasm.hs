-- Fasm target for x86
module Codegen.Fasm(fasm) where
import Codegen.Common

import BParser        (BBinary (..))
import Data.Bits
import Data.List
import Data.Maybe
import Data.Word
import Generator
import System.Process
import Data.Foldable

fasm = Target
       "fasm"
       buildRecipe
  
buildRecipe nC outputFileName sourceFiles objectFiles linkerFlags = do 
    traverse_ (\fileName -> do
                    runIfChanged nC [fileName]
                      (getFileName ".s" fileName)
                      (compileFile (return . asm) False fileName)
                    runIfChanged nC [getFileName ".s" fileName]
                      (getFileName ".o" fileName)
                      (prettyProcess $ readProcessWithExitCode "fasm"
                        [getFileName ".s" fileName] "")
              ) sourceFiles

    runIfChanged nC (objectFiles ++ map (getFileName ".o") sourceFiles)
       (takeWhile (/='.') outputFileName)
       (prettyProcess $ readProcessWithExitCode "gcc" (["-o", takeWhile (/='.') outputFileName, "-no-pie"] ++ map (getFileName ".o") sourceFiles ++ objectFiles ++ linkerFlags) "")
    return ()

asm :: IRProgram -> String
asm p = aProgramPrologue ++ "\n" ++
        concatMap (\x -> "extrn " ++ x ++ "\n") (extrns p) ++ "\n" ++
        concatMap aFunction (functions p) ++ "\n" ++
        concatMap aNakedFunctionSection (nakedFunctions p) ++ "\n" ++
        "section '.data' writeable" ++
        aGlobalVarSection (globalVars p) ++ "\n" ++
        aDataSection (staticData p)

aProgramPrologue :: String
aProgramPrologue = "format ELF64\n" ++
                   "section '.text' executable"

aDataSection :: [Word8] -> String
aDataSection a = if null a then ""
                 else "dat db " ++ intercalate "," (map show a)

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

aFunction :: Function -> String
aFunction f = aFunctionPrologue (funName f) (paramsCount f) (autoVarCount f) ++ "\n" ++
              concatMap (\x->aOp (funName f) (paramsCount f) (autoVarCount f) x ++ "\n") (body f) ++ "\n" ++
              aFunctionEpilogue (paramsCount f) (fromIntegral $ autoVarCount f)

aFunctionPrologue :: String -> Int -> Int -> String
aFunctionPrologue name countParam countAutoVars = "\npublic " ++ name ++ "\n" ++
                                                  name ++ ":\n" ++
                                                  "push rbp\n" ++ -- pushing frame pointer to stack
                                                  "sub rsp, " ++ show stackOffset ++ "\n" ++
                                                  "mov rbp, rsp\n" ++
                                                  if countParam==0 then []
                                                  else concat (zipWith storeVarOnStack ["rdi", "rsi", "rdx", "rcs", "r8", "r9"] [0..(countParam - 1)])
    where stackOffset = if mod ccc 16 == 0 then ccc else div ccc 16*16 + 16
          ccc = (countParam + countAutoVars)*8

aFunctionEpilogue :: Int -> Int -> String
aFunctionEpilogue countParam countAutoVars = "add rsp, " ++ show stackOffset ++ "\n" ++
                                             "pop rbp\n" ++
                                             "ret\n"
    where stackOffset = if mod ccc 16 == 0 then ccc else div ccc 16*16 + 16
          ccc = (countParam + countAutoVars)*8

storeVarOnStack :: String -> Int -> String
storeVarOnStack reg offset = "mov [rbp + " ++ show (offset*8) ++ "], " ++ reg ++ "\n"

loadVarInStack :: String -> Int -> String
loadVarInStack destReg offset = "mov " ++ destReg ++ ", [rbp + " ++ show (offset*8) ++ "]\n"

storeVarInMem :: String -> Int -> String
storeVarInMem reg ptrOffset = loadVarInStack "r9" ptrOffset ++
                              "mov [r9], " ++ reg ++
                              "\n; storing variable in memory"

loadVarInMem :: String -> Int -> String
loadVarInMem destReg ptrOffset = loadVarInStack "r9" ptrOffset ++
                                 "mov " ++ destReg ++ ", [r9]" ++
                                 "\n; loading variable in memory\n"

aOp :: String -> Int -> Int -> Op -> String
aOp funName countParam countAutoVars o = case o of
          Funcall offset fnLoc fnArgs -> concat (zipWith aArg ["rdi", "rsi", "rdx", "rcs", "r8", "r9"] fnArgs) ++
                                         fl fnLoc ++ "\n" ++
                                         storeVarOnStack "rax" (fromIntegral offset)
          OpBin operator resultAutoVar lhs rhs -> aBinary operator resultAutoVar lhs rhs
          AutoAssign loc arg -> aArg "rax" arg ++ storeVarOnStack "rax" (fromIntegral loc)
          MemoryAssign ptrLoc arg -> aArg "rax" arg ++ storeVarInMem "rax" (fromIntegral ptrLoc)
          ExternalAssign loc arg -> aArg "UNREACHABLE" arg ++
                                    "ADRP X1, _" ++ loc ++ "@GOTPAGE\n" ++
                                    "LDR X1, [X1, _" ++ loc ++ "@GOTPAGEOFF]\n" ++
                                    "STR X0, [X1, #0]\n"
          Index dest ptsArg offsetArg -> aArg "UNREACHABLE" ptsArg ++ aArg "UNREACHABLE" offsetArg ++
                                         "MOV X3, #8\n" ++
                                         "MUL X2, X2, X3\n" ++
                                         "ADD X0, X1, X2\n" ++
                                         storeVarOnStack "UNREACHABLE" (fromIntegral dest)
          Label labelN -> funName ++ show labelN ++ ":"
          JmpLabel labelN -> "jmp " ++ funName ++ show labelN
          JmpIfZeroLabel labelN arg -> aArg "rax" arg ++
                                      "cmp rax, 0\n" ++
                                      "je " ++ funName ++ show labelN
          Return Nothing -> aFunctionEpilogue countParam countAutoVars
          Return (Just arg) -> aArg "rax" arg ++
                               aFunctionEpilogue countParam countAutoVars
          UnaryNot dest arg -> aArg "UNREACHABLE" arg ++
                               "CMP X0, #0\n" ++
                               "CSET X0, EQ\n" ++
                               storeVarOnStack "UNREACHABLE" (fromIntegral dest)
          Negate dest arg -> aArg "UNREACHABLE" arg ++
                             "NEG X0, X0\n" ++
                             storeVarOnStack "UNREACHABLE" (fromIntegral dest)
          Asm a -> unlines a
          NoOp _ -> ""
    where fl (External s) = "call " ++ s
          fl a            = aArg "UNREACHABLE" a ++ "\n" ++ "BLR X16"

aArg :: String -> Arg -> String
aArg reg arg = case arg of
             DataOffset doff -> "mov " ++ reg ++ ", dat" ++ "\n" ++
                                "add " ++ reg ++ ", " ++ show doff ++ "\n"
             Literal a -> "mov " ++ reg ++ ", " ++ show a ++ "\n"
             AutoVar autoVarOffset -> loadVarInStack reg (fromIntegral autoVarOffset)
             Deref autoVarOffset -> loadVarInMem reg (fromIntegral autoVarOffset)
             External name -> "ADRP X" ++ show reg ++ ", _" ++ name ++ "@GOTPAGE\n" ++
                              "LDR X" ++ show reg ++ ", [X" ++ show reg ++ ", _" ++ name ++ "@GOTPAGEOFF]\n" ++
                              "LDR X" ++ show reg ++ ", [X" ++ show reg ++ "]\n"

aBinary :: BinOp -> Word -> Arg -> Arg -> String
aBinary binOp resultLoc lArg rArg = aArg "rax" lArg ++
                                    aArg "rbx" rArg ++
                                    (case binOp of
                                      Add             -> "add rax, rbx\n"
                                      Subtract        -> "sub rax, rbx\n"
                                      Multiply        -> "imul rax, rbx\n"
                                      Equal           -> "cmp rax, rbx\n" ++
                                                         "sete al\n" ++
                                                         "movzx rax, al\n"
                                      NotEqual        -> "CMP X1, X2\n" ++
                                                         "CSET X0, NE\n"
                                      LessThan        -> "CMP X1, X2\n" ++
                                                         "CSET X0, LT\n"
                                      MoreThan        -> "cmp rax, rbx\n" ++
                                                         "seta al\n" ++
                                                         "movzx rax, al\n"
                                      LessThanOrEqual -> "CMP X1, X2\n" ++
                                                         "CSET X0, LE\n"
                                      MoreThanOrEqual -> "CMP X1, X2\n" ++
                                                         "CSET X0, GE\n"
                                      Modulo          -> "SDIV X0, X1, X2\n" ++   -- suppose we are doing a%b. x2 holds a/b quotient
                                                         "MSUB X0, X0, X2, X1\n"  -- which is q then we do (q*b -a) which is mod
                                      Or              -> "or rax, rbx\n"
                                      Divide          -> "SDIV X0, X1, X2\n"
                                    ) ++
                                    storeVarOnStack "rax" (fromIntegral resultLoc)

