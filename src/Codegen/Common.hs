{-# LANGUAGE OverloadedStrings #-}
module Codegen.Common where

import qualified Data.Text as T
import           Generator

data Target = Target { targetName :: String, optimization :: Bool, output :: IRProgram -> IO T.Text }

newtype StateM state b = StateM { runStateM :: state -> (state, b) } deriving (Functor)

instance Applicative (StateM a) where
    pure a = StateM (,a)
    (StateM x) <*> (StateM y) = StateM $ \c -> let (cs, f) = x c
                                                   (cs',t) = y cs
                                               in (cs', f t)

instance Monad (StateM a) where
    x >>= y = StateM $ \c -> let (cs, input) = runStateM x c
                              in runStateM (y input) cs
