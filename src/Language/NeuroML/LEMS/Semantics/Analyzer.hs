{-|
Module      : Language.NeuroML.LEMS.Semantics.Analyzer
Description : Analyzes parse tree to generate model.
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

LEMS model after being parsed from XML.
-}
module Language.NeuroML.LEMS.Semantics.Analyzer where

import qualified Data.Map.Strict as M

import Language.NeuroML.LEMS.Semantics.Model

import qualified Language.NeuroML.LEMS.Parser.XMLParser as P
import qualified Language.NeuroML.LEMS.Parser.ParseTree as PT

import Data.Maybe
import Data.Functor














------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------

test file = do
  contents <- readFile file
  pt <- P.parseLemsXML contents
  putStrLn $ show pt
  

testLems lemsFile = test $ "/home/gautham/work/NeuroML/LEMS/examples/" ++ lemsFile
