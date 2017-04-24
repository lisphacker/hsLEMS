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

import Data.Map.Strict

-- | Definition of a dimension as a combinations of base SI units.
data Dimension = Dimension { dimName        :: String  -- ^ Name of the dimension
                           , dimMass        :: Int     -- ^ Mass (kilogram, kg)
                           , dimLength      :: Int     -- ^ Length (metre, m)
                           , dimTime        :: Int     -- ^ Time (second, s)
                           , dimCurrent     :: Int     -- ^ Electric current (ampere, A)
                           , dimTemperature :: Int     -- ^ Thermodynamic temperature (kelvin, K)
                           , dimQuantity    :: Int     -- ^ Quantity (mole, mol)
                           , dimLumInt      :: Int     -- ^ Luminous intensity (candela, cd)
                           } deriving (Show)

-- | Definition of a unit symbol as a scaled dimension.
data Unit = Unit { unitName      :: String     -- ^ Name of the unit
                 , unitSymbol    :: String     -- ^ Unit symbol
                 , unitDimension :: Dimension  -- ^ Name of the dimension being scaled
                 , unitPower10   :: Int        -- ^ Power of 10 scale value
                 , unitScale     :: Double     -- ^ Non-power of 10 scale value
                 , unitOffset    :: Double     -- ^ Non-zero offset
                 } deriving (Show)
