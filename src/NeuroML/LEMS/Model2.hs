module NeuroML.LEMS.Model2 where

  {-
-- | Type class for named objects
class Named a where
  name :: a -> String
  
-- | Type class for extended objects
class Extended a where
  extends :: a -> String
-}
  
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

{-
instance Named Dimension where
  name = dimName
-}

-- | Definition of a unit symbol as a scaled dimension.
data Unit = Unit { unitName      :: String  -- ^ Name of the unit
                 , unitSymbol    :: String  -- ^ Unit symbol
                 , unitDimension :: String  -- ^ Name of the dimension being scaled
                 , unitPower10   :: Int     -- ^ Power of 10 scale value
                 , unitScale     :: Double  -- ^ Non-power of 10 scale value
                 , unitOffset    :: Double  -- ^ Non-zero offset
                 } deriving (Show)

{-
instance Named Unit where
  name = unitName
-}

-- | Definition of a simulation target component.
data Target = Target { tgtComponent   :: String  -- ^ Name of the component used as a simulation target
                     , tgtReportsFile :: String  -- ^ Optional file for saving a short simulation report
                     , tgtTimesFile   :: String  -- ^ Optional file for saving simulation times
                     } deriving (Show)

-- | Definition of a global constant.
data Constant = Constant { cnstName      :: String  -- ^ Name of the constant
                         , cnstSymbol    :: String  -- ^ Symbol used for the constant in expressions
                         , cnstValue     :: String  -- ^ Value of the constant
                         , cnstDimension :: String  -- ^ Dimension of the constant
                         } deriving (Show)

-- | Include directive.
data Include = Include { includeFile :: String -- ^ Name of the file to be included
                       } deriving (Show)

-- | Definition of a dimension equivalence assertion. Used for validation of dimensions.
data Assertion = Assertion { assertDimension :: String  -- ^ Dimension being checked
                           , assertMatches   :: String  -- ^ Dimension expression to be matched
                           } deriving (Show)

-- | Definition of a parameter in a component type.
data Parameter = Parameter { paramName      :: String -- ^ Parameter name
                           , paramDimension :: String -- ^ Dimension of the parameter
                           } deriving (Show)

-- | Definition of a component type.
data ComponentType = ComponentType { compTypeName       :: String      -- ^ Name of the component type
                                   , compTypeExtends    :: String      -- ^ Name of the base component type
                                   , compTypeParameters :: [Parameter] -- ^ Component type parameters
                                   } deriving (Show)

{-
instance Named ComponentType where
  name = compTypeName

instance Extended ComponentType where
  extends = compTypeExtends
-}
