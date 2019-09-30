{-
Module      : Language.NeuroML.LEMS.Semantics.Analysis
Description : Analyzes parse tree to generate model.
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gauthamg@gmail.com
Stability   : experimental
Portability : POSIX

LEMS model after being parsed from XML.
-}
module Language.NeuroML.LEMS.Semantics.Analysis where

import Protolude
import Data.Maybe (fromJust)

import Control.Monad.State (get, put)

import Control.Lens

import qualified Data.Map.Strict as M (insert, lookup, notMember)

import Language.NeuroML.LEMS.Errors

import qualified Language.NeuroML.LEMS.Parser as P

import Language.NeuroML.LEMS.Semantics.Model
--import Language.NeuroML.LEMS.Semantics.Parser

import Language.NeuroML.LEMS.Monad (CompilerMonad, runCompilerMonad)

type AnalysisMonad a = CompilerMonad CompilerError Lems a

processPTDimensions :: [P.Dimension] -> AnalysisMonad ()
processPTDimensions ptDimensions  = forM_ ptDimensions $ \(P.Dimension name m l t c t' q l') -> do
  lems <- get
  let lems' = lemsDimensions %~ (M.insert name (Dimension name m l t c t' q l')) $ lems
  put lems'
  return lems'

processPTUnits :: [P.Unit] -> AnalysisMonad ()
processPTUnits ptUnits  = forM_ ptUnits $ \(P.Unit name sym dimName pow10 sc off) -> do
  lems <- get
  when (M.notMember dimName $ lems ^. lemsDimensions) $ throwError $ UnknownDimension dimName
  let dim = fromJust $ M.lookup dimName $ lems ^. lemsDimensions
  let lems' = lemsUnits %~ (M.insert name (Unit name sym dim pow10 sc off)) $ lems
  put lems'
  return lems'

{-
processPTConstants :: [P.Constant] -> AnalysisMonad ()
processConstants ptConstants = forM_ ptConstants $ \(P.Constant name sym dimName value) -> do
  lems <- get
  when (M.notMember dimName $ lems ^. lemsDimensions) $ throwError $ UnknownDimension dimName
  let key = if sym == "" then name else sym
-}
  
processParseTree :: P.Lems -> Either CompilerError Lems
processParseTree (P.Lems _ dimensions units _ _ _ _ _) =
  runCompilerMonad newModel $ do
    processPTDimensions dimensions
    processPTUnits units














{-

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

-}
