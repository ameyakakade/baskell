module Codegen.Common where

import Generator

data Codegen = Codegen { targetName :: String, optimization :: Bool, output :: IRProgram -> String }
