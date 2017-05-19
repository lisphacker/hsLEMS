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

import Data.Maybe
import Data.Functor
import Control.Monad as Monad

import Data.Text (pack, unpack)

import qualified Data.Map.Strict as M (empty, insert, lookup)

import qualified Language.NeuroML.LEMS.Parser as P

import Language.NeuroML.LEMS.Semantics.Model




processPTDimensions = processDims . P.lemsDimensions
  where processDims = foldl (\m d -> M.insert (P.dimName d) (convert d) m) M.empty
        convert d = let name = P.dimName d
                        m    = P.dimMass d
                        l    = P.dimLength d
                        t    = P.dimTime d
                        i    = P.dimCurrent d
                        k    = P.dimTemperature d
                        n    = P.dimQuantity d
                        j    = P.dimLumInt d
                    in Dimension name m l t i k n j

processPTUnits dimMap parseTree = processUnits (P.lemsUnits parseTree)
  where processUnits = foldl (\m u -> M.insert (P.unitSymbol u) (convert u) m) M.empty
        convert u = let name   = P.unitName u
                        symbol = P.unitSymbol u
                        dim    = case M.lookup (P.unitDimension u) dimMap of
                                   Just d  -> d
                                   Nothing -> error $ "Unknown dimension "
                        pow10  = P.unitPower10 u
                        scale  = P.unitScale u
                        offset = P.unitOffset u
                    in Unit name symbol dim pow10 scale offset






------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------

includeDirs = ["/home/gautham/work/NeuroML/LEMS/examples"]

test file = do
  maybeParseTree <- P.parseLemsXMLFile includeDirs file
  Monad.when (isNothing maybeParseTree) $ error "Unable to parse XML"
  let parseTree = fromJust maybeParseTree
      dimMap = processPTDimensions parseTree
      unitMap = processPTUnits dimMap parseTree
  putStrLn $ show $ M.lookup (pack "capacitance") dimMap
  putStrLn $ show $ M.lookup (pack "mV") unitMap
  

testLems lemsFile = test $ "/home/gautham/work/NeuroML/LEMS/examples/" ++ lemsFile
