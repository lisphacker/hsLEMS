{-|
Module      : Language.NeuroML.LEMS.Model
Description : Parsed LEMS model
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

LEMS model after being parsed from XML.
-}
module Language.NeuroML.LEMS.Model where

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
data Unit = Unit { unitName      :: String  -- ^ Name of the unit
                 , unitSymbol    :: String  -- ^ Unit symbol
                 , unitDimension :: String  -- ^ Name of the dimension being scaled
                 , unitPower10   :: Int     -- ^ Power of 10 scale value
                 , unitScale     :: Double  -- ^ Non-power of 10 scale value
                 , unitOffset    :: Double  -- ^ Non-zero offset
                 } deriving (Show)

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

-- | Specification of a parameter whose value has been fixed.
data Fixed = Fixed { fixedName  :: String -- ^ Fixed parameter name
                   , fixedValue :: String -- ^ Fixed parameter value
                   } deriving (Show)

-- | Specification of an exposed parameter (parameter visible from outside the component).
data Exposure = Exposure { expName      :: String -- ^ Exposure name
                         , expDimension :: String -- ^ Exposure dimension
                         } deriving (Show)

-- | Definition of an event port.
data EventPort = EventPort { epName      :: String -- ^ Event port name
                           , epDirection :: String -- ^ Direction (IN/OUT)
                           } deriving (Show)

-- | Definition of a state variable.
data StateVariable = StateVariable { svName      :: String -- ^ State variable name
                                   , svExposure  :: String -- ^ State variable exposure
                                   , svDimension :: String -- ^ State variable dimensions
                                   } deriving (Show)

-- | Definition of the time derivative of a state variable.
data TimeDerivative = TimeDerivative { tdVariable :: String -- ^ Variable name
                                     , tdValue    :: String -- ^ EXpression for the time derivative
                                     } deriving (Show)


data Action = StateAssignment { saVariable :: String -- ^ State variable to be assigned to
                              , saValue    :: String -- ^ Value/expression being assigned
                              }
            | EventOut { eoPort :: String } -- ^ Activate the specified output event port.
            deriving (Show)
            
-- | Definition of an event handler.
data OnEvent = OnEvent { oePort    :: String   -- ^ The port for which this handler responds to
                       , oeActions :: [Action] -- ^ List of action to perform in response to event
                       } deriving (Show)

-- | Definition of a dynamics container.
data Dynamics = Dynamics { dynStateVariables :: [StateVariable]   -- ^ State variable definitions.
                         , dynTimeDerivatives :: [TimeDerivative] -- ^ Derivatives for state variables.
                         , dynEventHandlers   :: [OnEvent]        -- ^ Event handlers
                         } deriving (Show)

-- | Definition of a component type.
data ComponentType = ComponentType { compTypeName            :: String      -- ^ Name of the component type
                                   , compTypeExtends         :: String      -- ^ Name of the base component type
                                   , compTypeParameters      :: [Parameter] -- ^ Component type parameters
                                   , compTypeFixedParameters :: [Fixed]     -- ^ Fixed parameters
                                   , compTypeExposures       :: [Exposure]  -- ^ Exposures
                                   , compTypeEventPorts      :: [EventPort] -- ^ Event ports
                                   , compTypeDynamics        :: Maybe Dynamics -- ^ Dynamics
                                   } deriving (Show)

-- | Definition of a container.
data Component = Component { compId      :: String  -- ^ Component id
                           , compName    :: String  -- ^ Component name
                           , compType    :: String  -- ^ Component type
                           , compExtends :: String  -- ^ Name of base component.
                           } deriving (Show)
-- | Main container.
data Lems = Lems { lemsIncludes   :: [Include]
                 , lemsDimensions :: [Dimension]
                 , lemsUnits      :: [Unit]
                 , lemsAssertions :: [Assertion]
                 , lemsConstants  :: [Constant]
                 , lemsCompTypes  :: [ComponentType]
                 , lemsComponents :: [Component]
                 , lemsTarget     :: Maybe Target
                 } deriving (Show)
