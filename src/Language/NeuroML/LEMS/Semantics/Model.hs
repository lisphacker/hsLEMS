{-|
Module      : Language.NeuroML.LEMS.Semantics.Model
Description : Model created after resolving LEMS semantics on the AST
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

LEMS model after being parsed from XML.
-}
module Language.NeuroML.LEMS.Semantics.Model where

import Data.Map.Strict as M

import Data.Text

-- | Definition of a dimension as a combinations of base SI units.
data Dimension = Dimension { dimName        :: Text -- ^ Name of the dimension
                           , dimMass        :: Int  -- ^ Mass (kilogram, kg)
                           , dimLength      :: Int  -- ^ Length (metre, m)
                           , dimTime        :: Int  -- ^ Time (second, s)
                           , dimCurrent     :: Int  -- ^ Electric current (ampere, A)
                           , dimTemperature :: Int  -- ^ Thermodynamic temperature (kelvin, K)
                           , dimQuantity    :: Int  -- ^ Quantity (mole, mol)
                           , dimLumInt      :: Int  -- ^ Luminous intensity (candela, cd)
                           } deriving (Show)
type DimensionMap = M.Map Text Dimension            -- ^ Map of names to dimensions

-- | Definition of a unit symbol as a scaled dimension.
data Unit = Unit { unitName      :: Text      -- ^ Name of the unit
                 , unitSymbol    :: Text      -- ^ Unit symbol
                 , unitDimension :: Dimension -- ^ Name of the dimension being scaled
                 , unitPower10   :: Int       -- ^ Power of 10 scale value
                 , unitScale     :: Double    -- ^ Non-power of 10 scale value
                 , unitOffset    :: Double    -- ^ Non-zero offset
                 } deriving (Show)
type UnitMap = M.Map Text Dimension           -- ^ Map of symbols to units



-- | Lems model
data Lems = Lems { lemsDimensions :: DimensionMap
                 , lemsUnits      :: UnitMap
                 } deriving (Show)
