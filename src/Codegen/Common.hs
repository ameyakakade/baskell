module Codegen.Common where

import Generator

data Target = Target { targetName :: String, optimization :: Bool, output :: IRProgram -> String }

data CodegenM a b = CodegenM { runCodegenM :: a -> (a, b) } deriving (Functor)

instance Applicative (CodegenM a) where
    pure a = CodegenM (,a)
    (CodegenM x) <*> (CodegenM y) = CodegenM $ \c -> let (cs, f) = x c
                                                         (cs',t) = y cs
                                                     in (cs', f t)

instance Monad (CodegenM a) where
    x >>= y = CodegenM $ \c -> let (cs, input) = runCodegenM x c
                              in runCodegenM (y input) cs
