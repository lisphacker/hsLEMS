{-|
Module      : Language.NeuroML.LEMS.Semantics.Monad
Description : Compiler monad
Copyright   : (c) Gautham Ganapathy, 2019
License     : BSD
Maintainer  : gauthamg@gmail.com
Stability   : experimental
Portability : POSIX

LEMS model after being parsed from XML.
-}
module Language.NeuroML.LEMS.Semantics.Monad where
    
import Control.Monad.Except (ExceptT)
import Control.Monad.State (StateT)

type CompilerMonad e s m = ExceptT e (StateT s m)
