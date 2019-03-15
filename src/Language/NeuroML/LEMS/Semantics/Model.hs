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

import Protolude

import Data.Map.Strict as M

type NumericValue = Double

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
data Unit = Unit { unitName      :: Text         -- ^ Name of the unit
                 , unitSymbol    :: Text         -- ^ Unit symbol
                 , unitDimension :: Dimension    -- ^ Name of the dimension being scaled
                 , unitPower10   :: Int          -- ^ Power of 10 scale value
                 , unitScale     :: NumericValue -- ^ Non-power of 10 scale value
                 , unitOffset    :: NumericValue -- ^ Non-zero offset
                 } deriving (Show)
type UnitMap = M.Map Text Unit                   -- ^ Map of symbols to units

-- | Type class of dimensioned quantities
class DimensionedQuantity a where
  qtySIValue   :: a -> NumericValue
  qtyDimension :: a -> Dimension

-- | Definition of a constant value
-- | Definition of a global/local constant.
data Constant = Constant { cnstName      :: Text         -- ^ Name of the constant
                         , cnstDimension :: Dimension    -- ^ Dimension of the constant
                         , cnstValue     :: NumericValue -- ^ Value of the constant
                         , cnstUnit      :: Unit         -- ^ Unit of the constant
                         } deriving (Show)
type ConstantMap = M.Map Text Constant                   -- ^ Map of names to constants

instance DimensionedQuantity Constant where
  qtySIValue   (Constant _ _ v u) = v * (10.0 ** (fromIntegral $ unitPower10 u)) + unitOffset u
  qtyDimension (Constant _ d _ _) = d

-- | Lems model
data Lems = Lems { lemsDimensions :: DimensionMap
                 , lemsUnits      :: UnitMap
                 , lemsConstants  :: ConstantMap
                 } deriving (Show)
