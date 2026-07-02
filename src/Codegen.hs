module Codegen(targets, module Codegen.Common) where

import Codegen.Common
import Codegen.GasDarwinAArch64
import Codegen.GasAArch64
import Generator

targets :: [Target]
targets = [
  gasDarwinAArch64
  ]
