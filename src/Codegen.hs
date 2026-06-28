module Codegen(targets, module Codegen.Common) where

import Codegen.Common
import Codegen.GasDarwinAArch64
import Generator

targets :: [Codegen]
targets = [
  gasDarwinAArch64
  ]
