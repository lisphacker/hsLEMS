{-|
Module      : Language.NeuroML.LEMS.Parser.AST
Description : Parsed LEMS AST
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

LEMS model after being parsed from XML.
-}
module Language.NeuroML.LEMS.Parser.AST where

import Data.Map.Strict

import Text.XML.HXT.Core hiding (xread)
import Data.Tree.NTree.TypeDefs

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

-- | Definition of a derived parameter.
data DerivedParameter = DerivedParameter { dpName      :: String -- ^ Parameter name
                                         , dpDimension :: String -- ^ Dimension of the parameter
                                         , dpSelect    :: String -- ^ Target selection path
                                         , dpValue     :: String -- ^ Valu expression
                                         } deriving (Show)

-- | Definition of a component reference
data ComponentReference = ComponentReference { crefName :: String -- ^ Component reference parameter name.
                                             , crefType :: String -- ^ Type of the component being referenced.
                                         } deriving (Show)

-- | Definition of a link entry.
data Link = Link { lnkName :: String -- ^ Name of the link
                 , lnkType :: String -- ^ Type of the linked component
                 } deriving (Show)

-- | Specification of an exposed parameter (parameter visible from outside the component).
data Exposure = Exposure { expName      :: String -- ^ Exposure name
                         , expDimension :: String -- ^ Exposure dimension
                         } deriving (Show)

-- | Specification of a required parameter (parameter defined in sub-component types).
data Requirement = Requirement { reqName      :: String -- ^ Requirement name
                               , reqDimension :: String -- ^ Requirement dimension
                               } deriving (Show)

-- | Definition of an event port.
data EventPort = EventPort { epName      :: String -- ^ Event port name
                           , epDirection :: String -- ^ Direction (IN/OUT)
                           } deriving (Show)

-- | Definition of a text entry.
data Text = Text { txtName :: String -- ^ Name of the text parameter
                 } deriving (Show)

-- | Definition of a path entry.
data Path = Path { pthName :: String -- ^ Name of the path parameter
                 } deriving (Show)

-- | Definition of a child  of a given type.
data Child = Child { chName :: String -- ^ Name
                   , chType :: String -- ^ Type
                   } deriving (Show)

-- | Definition of children of a given type
data Children = Children { chnName :: String -- ^ Name
                         , chnType :: String -- ^ Type
                         } deriving (Show)

-- | Definition of a state variable.
data StateVariable = StateVariable { svName      :: String -- ^ State variable name
                                   , svExposure  :: String -- ^ State variable exposure
                                   , svDimension :: String -- ^ State variable dimensions
                                   } deriving (Show)

-- | Definition of a derived variable.
data DerivedVariable = DerivedVariable { dvName      :: String -- ^ Derived variable name
                                       , dvExposure  :: String -- ^ Derived variable exposure
                                       , dvDimension :: String -- ^ Derived variable dimensions
                                       , dvValue     :: String -- ^ Value expression
                                       , dvSelect    :: String -- ^ Target collection selection
                                       , dvReduce    :: String -- ^ Reduce operation
                                       , dvRequired  :: String -- ^ Set to OK if variable is required
                                       } deriving (Show)

-- | Definition of the time derivative of a state variable.
data TimeDerivative = TimeDerivative { tdVariable :: String -- ^ Variable name
                                     , tdValue    :: String -- ^ EXpression for the time derivative
                                     } deriving (Show)


data Action = StateAssignment { saVariable :: String -- ^ State variable to be assigned to
                              , saValue    :: String -- ^ Value/expression being assigned
                              }
            | EventOut { eoPort :: String } -- ^ Activate the specified output event port
            | Transition { trRegime :: String } -- ^ Regime to transition to
            deriving (Show)

{-
-- | Definition of a startup handler.
data OnStart = OnStart { osActions :: [Action] -- ^ List of action to perform on startup
                       } deriving (Show)

-- | Definition of a condition handler.
data OnCondition = OnCondition { ocTest :: String      -- ^ The port for which this handler responds to
                               , ocActions :: [Action] -- ^ List of action to perform in response to event
                               } deriving (Show)

-- | Definition of an event port handler.
data OnEvent = OnEvent { oePort    :: String   -- ^ The port for which this handler responds to
                       , oeActions :: [Action] -- ^ List of action to perform in response to event
                       } deriving (Show)
-}

-- | Definition of an event handler (startup/condition/port)
data EventHandler = OnStart { osActions :: [Action] -- ^ List of action to perform on startup
                            }
                  | OnCondition { ocTest :: String      -- ^ The port for which this handler responds to
                                , ocActions :: [Action] -- ^ List of action to perform in response to event
                                }
                  | OnEvent { oevPort    :: String   -- ^ The port for which this handler responds to
                            , oevActions :: [Action] -- ^ List of action to perform in response to event
                            }
                  | OnEntry { oenActions :: [Action] -- ^ List of action to perform in response to event
                            }
                  deriving (Show)

-- | Definition of a dynamics regime.
data Regime = Regime { regName             :: String            -- ^ Regime name
                     , regInitial          :: String            -- ^ "true" if active regime on startup
                     , regStateVariables   :: [StateVariable]   -- ^ State variable definitions.
                     , regTimeDerivatives  :: [TimeDerivative]  -- ^ Derivatives for state variables.
                     , regDerivedVariables :: [DerivedVariable] -- ^ Dervied variables.
                     , regEventHandlers    :: [EventHandler]    -- ^ Event handlers
                     } deriving (Show)

-- | Definition of a kinetics scheme.
data KineticScheme = KineticScheme { ksName          :: String -- ^ Scheme name
                                   , ksNodes         :: String -- ^ Name of object list for nodes
                                   , ksStateVariable :: String -- ^ Name of the scheme state variable for each node
                                   , ksEdges         :: String -- ^ Name of object list for edges
                                   , ksEdgeSource    :: String -- ^ Name of the parameter specifying edge source
                                   , ksEdgeTarget    :: String -- ^ Name of the parameter specifying edge target
                                   , ksForwardRate   :: String -- ^ Name of the parameter specifying forward rate
                                   , ksReverseRate   :: String -- ^ Name of the parameter specifying reverse rate
                                   } deriving (Show)
                     
-- | Definition of a dynamics container.
data Dynamics = Dynamics { dynStateVariables   :: [StateVariable]   -- ^ State variable definitions
                         , dynTimeDerivatives  :: [TimeDerivative]  -- ^ Derivatives for state variables
                         , dynDerivedVariables :: [DerivedVariable] -- ^ Dervied variables
                         , dynEventHandlers    :: [EventHandler]    -- ^ Event handlers
                         , dynRegimes          :: [Regime]          -- ^ Dynamics regimes
                         , dynKineticSchemes   :: [KineticScheme]   -- ^ Kinetic schemes
                         } deriving (Show)

-- | Definition of a single child instance spec.
data ChildInstance = ChildInstance { ciComponent :: String -- ^ Name of parameter to be used as component reference
                                   } deriving (Show)

-- | Definition of a multiple child instances spec.
data MultiInstantiate = MultiInstantiate { miComponent :: String -- ^ Name of parameter to be used as component reference
                                         , miNumber    :: String -- ^ Number of instances
                                         } deriving (Show)

-- | Definition of an event connection.
data EventConnection = EventConnection { evcFrom :: String              -- ^ Variable containing link to source component
                                       , evcTo   :: String              -- ^ Variable containing link to target component
                                       , evcSourcePort :: String        -- ^ Source port
                                       , evcTargetPort :: String        -- ^ Target port
                                       , evcReceiver :: String          -- ^ Event receiver component (Overrides "to")
                                       , evcReceiverContainer :: String -- ^ Event receiver container
                                       } deriving (Show)

-- | Definition of a component reference variable assigned a single component.
data With = With { wthInstance :: String -- ^ Name of link parameter or path to instance
                 , wthAs       :: String -- ^ Variable name
                 } deriving (Show)
            
-- | Definition of a component reference variable iterating over a list of components.
data ForEach = ForEach { feInstances        :: String             -- ^ Name of link parameter or path to instance collection
                       , feAs               :: String             -- ^ Variable name
                       , feForEach          :: [ForEach]          -- ^ Nested for each statements
                       , feEventConnections :: [EventConnection]  -- ^ Event connections
                       } deriving (Show)
            
-- | Definition of a structural container.
data Structure = Structure { strChildInstances   :: [ChildInstance]    -- ^ Single component instantiations
                           , strMultiInstances   :: [MultiInstantiate] -- ^ Multiple component instantiations
                           , strEventConnection  :: [EventConnection]  -- ^ Event connections
                           , strWith             :: [With]             -- ^ Variable assignments
                           , strForEach          :: [ForEach]          -- ^ Variable iterations
                           } deriving (Show)

-- | Definition of a record spec.
data Record = Record { recQuantity  :: String -- ^ Name of the parameter containing path to the quantity to be recorded
                     , recTimeScale :: String -- ^ Name of the parameter containing timeScale value
                     , recScale     :: String -- ^ Name of the parameter containing a scale value for the recorded quantity
                     , recColor     :: String -- ^ Name of the parameter specifying the color (for plots)
                     } deriving (Show)

-- | Definition of a data writer.
data DataWriter = DataWriter { dwPath     :: String -- ^ Name of the parameter containing path of the file to be written to
                             , dwFilename :: String -- ^ Name of the parameter containing name of the file to be written to
                             } deriving (Show)
                 
-- | Definition of a data display.
data DataDisplay = DataDisplay { ddTitle      :: String -- ^ Name of the parameter containing title of the plot
                               , ddDataRegion :: String -- ^ Name of the parameter containing x,y-extents of the plot
                             } deriving (Show)
                 
-- | Definition of a simulation run spec.
data Run = Run { runComponent :: String -- ^ Name of the parameter containing title of the plot
               , runVariable  :: String -- ^ Name of the parameter specifying name of the time variable
               , runIncrement :: String -- ^ Name of the parameter specifying time increment
               , runTotal     :: String -- ^ Name of the parameter specifying total time of the simulation
               } deriving (Show)

-- | Definition of a simulation spec container.
data Simulation = Simulation { simRecorders    :: [Record]      -- ^ Quantities to be recorded
                             , simDataWriters  :: [DataWriter]  -- ^ File data writers
                             , simDataDisplays :: [DataDisplay] -- ^ Data plotters
                             , simRun          :: [Run]         -- ^ Run specs
                             } deriving (Show)
                 
-- | Definition of a component type.
data ComponentType = ComponentType { compTypeName              :: String               -- ^ Name of the component type
                                   , compTypeExtends           :: String               -- ^ Name of the base component type
                                   , compTypeParameters        :: [Parameter]          -- ^ Component type parameters
                                   , compTypeFixedParameters   :: [Fixed]              -- ^ Fixed parameters
                                   , compTypeDerivedParameters :: [DerivedParameter]   -- ^ Derived parameters
                                   , compTypeCompRefs          :: [ComponentReference] -- ^ Component references
                                   , compTypeLinks             :: [Link]               -- ^ Link definitions
                                   , compTypeExposures         :: [Exposure]           -- ^ Exposures
                                   , compTypeRequirements      :: [Requirement]        -- ^ Requirements
                                   , compTypeChildDefs         :: [Child]              -- ^ Child definitions
                                   , compTypeChildrenDefs      :: [Children]           -- ^ Children definitions
                                   , compTypeEventPorts        :: [EventPort]          -- ^ Event ports
                                   , compTypeTexts             :: [Text]               -- ^ Text definitions
                                   , compTypePaths             :: [Path]               -- ^ Path definitions
                                   , compTypeDynamics          :: Maybe Dynamics       -- ^ Dynamics
                                   , compTypeStructure         :: Maybe Structure      -- ^ Structure
                                   , compTypeSimulation        :: Maybe Simulation     -- ^ Simulation specs
                                   } deriving (Show)

-- | Definition of a container.
data Component = Component { compId         :: String            -- ^ Component id
                           , compName       :: String            -- ^ Component name
                           , compType       :: String            -- ^ Component type
                           , compExtends    :: String            -- ^ Name of base component
                           , compParameters :: Map String String -- ^ Parameters
                           --, compParameters :: [XmlTree]
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
