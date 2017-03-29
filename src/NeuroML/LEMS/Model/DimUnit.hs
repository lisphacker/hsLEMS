module NeuroML.LEMS.Model.DimUnit where

import NeuroML.LEMS.Model.TypeClasses

-- | Defines a dimension as a combinations of base SI units.
data Dimension = Dimension { dimName     :: String  -- ^ Name of the dimension
                           , mass        :: Int     -- ^ Mass (kilogram, kg)
                           , length      :: Int     -- ^ Length (metre, m)
                           , time        :: Int     -- ^ Time (second, s)
                           , current     :: Int     -- ^ Electric current (ampere, A)
                           , temperature :: Int     -- ^ Thermodynamic temperature (kelvin, K)
                           , quantity    :: Int     -- ^ Quantity (mole, mol)
                           , lumint      :: Int     -- ^ Luminous intensity (candela, cd)
                           } deriving (Show)

instance Named Dimension where
  name = dimName

-- |  Defines a unit as a scaled dimension.
data Unit = Unit { unitName  :: String  -- ^ Name of the unit
                 , symbol    :: String  -- ^ Unit symbol
                 , dimension :: String  -- ^ Name of the dimension being scaled
                 , power10   :: Int     -- ^ Power of 10 scale value
                 , scale     :: Double  -- ^ Non-power of 10 scale value
                 , offset    :: Double  -- ^ Non-zero offset
                 } deriving (Show)

instance Named Unit where
  name = unitName
