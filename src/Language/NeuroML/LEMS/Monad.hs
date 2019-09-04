{-|
Module      : Language.NeuroML.LEMS.Monad
Description : Compiler monad
Copyright   : (c) Gautham Ganapathy, 2019
License     : BSD
Maintainer  : gauthamg@gmail.com
Stability   : experimental
Portability : POSIX

LEMS model after being parsed from XML.
-}
module Language.NeuroML.LEMS.Monad where

import Protolude

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Identity (Identity, runIdentity)

type CompilerMonad e s = ExceptT e (StateT s Identity) Void

runCompilerMonad :: s -> CompilerMonad e s -> Either e s
runCompilerMonad s = runIdentity . flip evalStateT s . runExceptT
                     
                         
