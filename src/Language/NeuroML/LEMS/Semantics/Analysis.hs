{-|
Module      : Language.NeuroML.LEMS.Semantics.Analyzer
Description : Analyzes parse tree to generate model.
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gauthamg@gmail.com
Stability   : experimental
Portability : POSIX

LEMS model after being parsed from XML.
-}
module Language.NeuroML.LEMS.Semantics.Analyzer where

import Protolude
import Protolude.Error

import Language.NeuroML.LEMS.Errors

import Data.String

import Data.Maybe
import Data.Functor
import Control.Monad as Monad
import Control.Monad.Extra as Monad

import Data.Text (pack, unpack)

import qualified Data.Map.Strict as M (empty, insert, lookup)

import qualified Language.NeuroML.LEMS.Parser as P

import Language.NeuroML.LEMS.Semantics.Model
import Language.NeuroML.LEMS.Semantics.Parser


processPTDimensions :: P.Lems -> DimensionMap
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

processPTUnits :: DimensionMap -> P.Lems -> Either CompilerError UnitMap
processPTUnits dimMap parseTree = processUnits (P.lemsUnits parseTree)
  where processUnits = foldM insertIntoMap M.empty
        insertIntoMap m u = let maybeu' = resolveUnit u
                            in case maybeu' of
                                 Left e   -> Left e
                                 Right u' -> Right $ M.insert (P.unitSymbol u) u' m
        resolveUnit u = let name   = P.unitName u
                            symbol = P.unitSymbol u
                            dimName = P.unitDimension u
                            pow10  = P.unitPower10 u
                            scale  = P.unitScale u
                            offset = P.unitOffset u
                        in case M.lookup dimName dimMap of
                             Nothing  -> Left $ UnknownDimension dimName
                             Just dim -> Right $ Unit name symbol dim pow10 scale offset

processPTConstants :: DimensionMap -> UnitMap -> P.Lems -> Either CompilerError ConstantMap
processPTConstants dimMap unitMap parseTree = processConstants (P.lemsConstants parseTree)
  where processConstants = foldM insertIntoMap M.empty
        insertIntoMap m c = let maybec' = resolveConstant c
                            in case maybec' of
                                 Left e   -> Left e
                                 Right c' -> Right $ M.insert (P.cnstName c) c' m
        resolveConstant c = let name   = P.cnstName c
                                dimName = P.cnstDimension c
                                valueStr = P.cnstValue c
                        in case M.lookup dimName dimMap of
                             Nothing  -> Left $ UnknownDimension dimName
                             Just dim -> case parseValue unitMap valueStr of
                                           Nothing            -> Left $ InvalidValue valueStr
                                           Just (value, unit) -> Right $ Constant name dim value unit


type ModelState = State (Either CompilerError Lems)

{-
processPTComponentType :: P.Lems -> P.ComponentType -> ModelState ()
processPTComponentType parseTree compType = 
  eitherM
    (\error -> return error)
    (\model -> return model)

processPTComponentTypes :: P.Lems -> ModelState ()
processPTComponentTypes parseTree = forM_ (P.lemsComponentTypes parseTree) $ \ct -> processPTComponentType parseTree ct
-}

processParseTree :: P.Lems -> Either CompilerError Lems
processParseTree parseTree = let dimMap = processPTDimensions parseTree
                             in do unitMap <- processPTUnits dimMap parseTree
                                   cnstMap <- processPTConstants dimMap unitMap parseTree
                                   Right $ Lems dimMap unitMap cnstMap M.empty

------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------
{-
strout :: String -> IO ()
strout = putStrLn

includeDirs :: [String]
includeDirs = ["/home/gautham/work/NeuroML/LEMS/examples"]

test :: String -> IO ()
test file = do
  maybeParseTree <- P.parseLemsXMLFile includeDirs file
  Monad.when (isNothing maybeParseTree) $ error "Unable to parse XML"
  let parseTree = fromJust maybeParseTree
      dimMap = processPTDimensions parseTree
      unitMap = processPTUnits dimMap parseTree
  strout $ show $ M.lookup (pack "capacitance") dimMap
  strout $ show $ M.lookup (pack "mV") unitMap
  

testLems :: [Char] -> IO ()
testLems lemsFile = test $ "/home/gautham/work/NeuroML/LEMS/examples/" ++ lemsFile
-}
