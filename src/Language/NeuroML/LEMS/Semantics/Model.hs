{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Language.NeuroML.LEMS.Semantics.Model
Description : Model created after resolving LEMS semantics on the AST
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gauthamg@gmail.com
Stability   : experimental
Portability : POSIX

LEMS model after being parsed from XML.
-}
module Language.NeuroML.LEMS.Semantics.Model where

import Protolude

import Control.Lens.TH (makeLenses)
import qualified Data.Map.Strict as M

type NumericValue = Double

-- | Definition of a dimension as a combinations of base SI units.
data Dimension = Dimension { _dimName        :: Text -- ^ Name of the dimension
                           , _dimMass        :: Int  -- ^ Mass (kilogram, kg)
                           , _dimLength      :: Int  -- ^ Length (metre, m)
                           , _dimTime        :: Int  -- ^ Time (second, s)
                           , _dimCurrent     :: Int  -- ^ Electric current (ampere, A)
                           , _dimTemperature :: Int  -- ^ Thermodynamic temperature (kelvin, K)
                           , _dimQuantity    :: Int  -- ^ Quantity (mole, mol)
                           , _dimLumInt      :: Int  -- ^ Luminous intensity (candela, cd)
                           } deriving (Show)
makeLenses ''Dimension
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

-- | Definition of a global/local constant.
data Constant = Constant { cnstName      :: Text         -- ^ Name of the constant
                         , cnstSymbol    :: Text         -- ^ Symbol used to refer to the constant
                         , cnstDimension :: Dimension    -- ^ Dimension of the constant
                         , cnstValue     :: NumericValue -- ^ Value of the constant
                         , cnstUnit      :: Unit         -- ^ Unit of the constant
                         } deriving (Show)
type ConstantMap = M.Map Text Constant                   -- ^ Map of names to constants

instance DimensionedQuantity Constant where
  qtySIValue   (Constant _ _ v u) = v * (10.0 ** (fromIntegral $ unitPower10 u)) + unitOffset u
  qtyDimension (Constant _ d _ _) = d


-- | Definition of a component type.
data ComponentType = ComponentType { compTypeName    :: Text          -- ^ Name of the component type
                                   , compTypeExtends :: ComponentType -- ^ Base component type
                                   } deriving (Show)
type ComponentTypeMap = M.Map Text ComponentType

-- | Lems model
data Lems = Lems { _lemsDimensions     :: DimensionMap        -- ^ Map of dimensions
                 , _lemsUnits          :: UnitMap             -- ^ Map of units
                 , _lemsConstants      :: ConstantMap         -- ^ Map of constants
                 , _lemsComponentTypes :: ComponentTypeMap    -- ^ Map of component types
                 } deriving (Show)
makeLenses ''Lems
newModel :: Lems
newModel = Lems M.empty M.empty M.empty M.empty
