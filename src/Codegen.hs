module Codegen(targets, module Codegen.Common) where

import Codegen.Common
import Codegen.Fasm
import Codegen.GasAArch64
import Codegen.GasDarwinAArch64
import Generator

targets :: [Target]
targets = [
    gasDarwinAArch64,
    gasAArch64,
    fasm
    ]
