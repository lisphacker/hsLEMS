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
import Control.Monad.State (StateT, runStateT)
import Control.Monad.Identity (Identity, runIdentity)

type CompilerMonad e s a = ExceptT e (StateT s Identity) a

runCompilerMonad :: s -> CompilerMonad e s a -> Either e s
runCompilerMonad s m = let (ea, s') = (runIdentity . flip runStateT s . runExceptT) m
                       in case ea of
                            Left e  -> Left e
                            Right _ -> Right s'
