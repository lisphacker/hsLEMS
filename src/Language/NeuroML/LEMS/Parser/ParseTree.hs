{-|
Module      : Language.NeuroML.LEMS.Parser.ParseTree
Description : LEMS parse tree
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

LEMS model after being parsed from XML.
-}
module Language.NeuroML.LEMS.Parser.ParseTree
  ( Dimension(..)
  , Unit(..)
  , Target(..)
  , Constant(..)
  , Include(..)
  , Assertion(..)
  , Parameter(..)
  , Fixed(..)
  , DerivedParameter(..)
  , ComponentReference(..)
  , Link(..)
  , Exposure(..)
  , Requirement(..)
  , EventPort(..)
  , Txt(..)
  , Path(..)
  , Child(..)
  , Children(..)
  , StateVariable(..)
  , DerivedVariable(..)
  , TimeDerivative(..)
  , Action (StateAssignment, EventOut, Transition)
  , EventHandler (OnStart, OnCondition, OnEvent, OnEntry)
  , Regime(..)
  , KineticScheme(..)
  , Dynamics(..)
  , ChildInstance(..)
  , MultiInstantiate(..)
  , EventConnection(..)
  , With(..)
  , ForEach(..)
  , Structure(..)
  , Record(..)
  , DataWriter(..)
  , DataDisplay(..)
  , Run(..)
  , Simulation(..)
  , ComponentType(..)
  , Component(..)
  , Lems(..)
  ) where

import Protolude

import Data.Map.Strict

import Text.XML.HXT.Core hiding (xread)
import Data.Tree.NTree.TypeDefs

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

-- | Definition of a unit symbol as a scaled dimension.
data Unit = Unit { unitName      :: Text   -- ^ Name of the unit
                 , unitSymbol    :: Text   -- ^ Unit symbol
                 , unitDimension :: Text   -- ^ Name of the dimension being scaled
                 , unitPower10   :: Int    -- ^ Power of 10 scale value
                 , unitScale     :: Double -- ^ Non-power of 10 scale value
                 , unitOffset    :: Double -- ^ Non-zero offset
                 } deriving (Show)

-- | Definition of a simulation target component.
data Target = Target { tgtComponent   :: Text -- ^ Name of the component used as a simulation target
                     , tgtReportsFile :: Text -- ^ Optional file for saving a short simulation report
                     , tgtTimesFile   :: Text -- ^ Optional file for saving simulation times
                     } deriving (Show)

-- | Definition of a global constant.
data Constant = Constant { cnstName      :: Text -- ^ Name of the constant
                         , cnstSymbol    :: Text -- ^ Symbol used for the constant in expressions
                         , cnstValue     :: Text -- ^ Value of the constant
                         , cnstDimension :: Text -- ^ Dimension of the constant
                         } deriving (Show)

-- | Include directive.
data Include = Include { includeFile :: Text -- ^ Name of the file to be included
                       } deriving (Show)

-- | Definition of a dimension equivalence assertion. Used for validation of dimensions.
data Assertion = Assertion { assertDimension :: Text -- ^ Dimension being checked
                           , assertMatches   :: Text -- ^ Dimension expression to be matched
                           } deriving (Show)

-- | Definition of a parameter in a component type.
data Parameter = Parameter { paramName      :: Text -- ^ Parameter name
                           , paramDimension :: Text -- ^ Dimension of the parameter
                           } deriving (Show)

-- | Specification of a parameter whose value has been fixed.
data Fixed = Fixed { fixedName  :: Text  -- ^ Fixed parameter name
                   , fixedValue :: Text  -- ^ Fixed parameter value
                   } deriving (Show)

-- | Definition of a derived parameter.
data DerivedParameter = DerivedParameter { dpName      :: Text -- ^ Parameter name
                                         , dpDimension :: Text -- ^ Dimension of the parameter
                                         , dpSelect    :: Text -- ^ Target selection path
                                         , dpValue     :: Text -- ^ Valu expression
                                         } deriving (Show)

-- | Definition of a component reference
data ComponentReference = ComponentReference { crefName :: Text -- ^ Component reference parameter name.
                                             , crefType :: Text -- ^ Type of the component being referenced.
                                         } deriving (Show)

-- | Definition of a link entry.
data Link = Link { lnkName :: Text -- ^ Name of the link
                 , lnkType :: Text -- ^ Type of the linked component
                 } deriving (Show)

-- | Specification of an exposed parameter (parameter visible from outside the component).
data Exposure = Exposure { expName      :: Text -- ^ Exposure name
                         , expDimension :: Text -- ^ Exposure dimension
                         } deriving (Show)

-- | Specification of a required parameter (parameter defined in sub-component types).
data Requirement = Requirement { reqName      :: Text -- ^ Requirement name
                               , reqDimension :: Text -- ^ Requirement dimension
                               } deriving (Show)

-- | Definition of an event port.
data EventPort = EventPort { epName      :: Text -- ^ Event port name
                           , epDirection :: Text -- ^ Direction (IN/OUT)
                           } deriving (Show)

-- | Definition of a text entry.
data Txt = Txt { txtName :: Text -- ^ Name of the text parameter
                 } deriving (Show)

-- | Definition of a path entry.
data Path = Path { pthName :: Text -- ^ Name of the path parameter
                 } deriving (Show)

-- | Definition of a child  of a given type.
data Child = Child { chName :: Text -- ^ Name
                   , chType :: Text -- ^ Type
                   } deriving (Show)

-- | Definition of children of a given type
data Children = Children { chnName :: Text -- ^ Name
                         , chnType :: Text -- ^ Type
                         } deriving (Show)

-- | Definition of a state variable.
data StateVariable = StateVariable { svName      :: Text -- ^ State variable name
                                   , svExposure  :: Text -- ^ State variable exposure
                                   , svDimension :: Text -- ^ State variable dimensions
                                   } deriving (Show)

-- | Definition of a derived variable.
data DerivedVariable = DerivedVariable { dvName      :: Text -- ^ Derived variable name
                                       , dvExposure  :: Text -- ^ Derived variable exposure
                                       , dvDimension :: Text -- ^ Derived variable dimensions
                                       , dvValue     :: Text -- ^ Value expression
                                       , dvSelect    :: Text -- ^ Target collection selection
                                       , dvReduce    :: Text -- ^ Reduce operation
                                       , dvRequired  :: Text -- ^ Set to OK if variable is required
                                       } deriving (Show)

-- | Definition of the time derivative of a state variable.
data TimeDerivative = TimeDerivative { tdVariable :: Text -- ^ Variable name
                                     , tdValue    :: Text -- ^ EXpression for the time derivative
                                     } deriving (Show)

-- | Definition of an action performed in reponse to an event.
data Action = StateAssignment { saVariable :: Text -- ^ State variable to be assigned to
                              , saValue    :: Text -- ^ Value/expression being assigned
                              } -- ^ Assign a new value to a state variable
              
              -- | Output an event on the specified event port
            | EventOut { eoPort :: Text -- ^ Activate the specified output event port
                       }
              -- | Switch to a new dynamics regime
            | Transition { trRegime :: Text -- ^ Regime to transition to
                         }
            deriving (Show)

-- | Definition of an event handler (startup / condition / event / regime entry)
data EventHandler = OnStart { osActions :: [Action] -- ^ List of action to perform on startup
                            } -- ^ Event handler executed on component startup
                    
                    -- | Event handler executed on condition becoming true
                  | OnCondition { ocTest :: Text        -- ^ The port for which this handler responds to
                                , ocActions :: [Action] -- ^ List of action to perform in response to event
                                }
                    -- | Event handler executed on receiving an specific event
                  | OnEvent { oevPort    :: Text     -- ^ The port for which this handler responds to
                            , oevActions :: [Action] -- ^ List of action to perform in response to event
                            }
                    -- | Event handler executed on entering a dynamics regime
                  | OnEntry { oenActions :: [Action] -- ^ List of action to perform in response to event
                            }
                  deriving (Show)

-- | Definition of a dynamics regime.
data Regime = Regime { regName             :: Text              -- ^ Regime name
                     , regInitial          :: Text              -- ^ "true" if active regime on startup
                     , regStateVariables   :: [StateVariable]   -- ^ State variable definitions.
                     , regTimeDerivatives  :: [TimeDerivative]  -- ^ Derivatives for state variables.
                     , regDerivedVariables :: [DerivedVariable] -- ^ Dervied variables.
                     , regEventHandlers    :: [EventHandler]    -- ^ Event handlers
                     } deriving (Show)

-- | Definition of a kinetics scheme.
data KineticScheme = KineticScheme { ksName          :: Text -- ^ Scheme name
                                   , ksNodes         :: Text -- ^ Name of object list for nodes
                                   , ksStateVariable :: Text -- ^ Name of the scheme state variable for each node
                                   , ksEdges         :: Text -- ^ Name of object list for edges
                                   , ksEdgeSource    :: Text -- ^ Name of the parameter specifying edge source
                                   , ksEdgeTarget    :: Text -- ^ Name of the parameter specifying edge target
                                   , ksForwardRate   :: Text -- ^ Name of the parameter specifying forward rate
                                   , ksReverseRate   :: Text -- ^ Name of the parameter specifying reverse rate
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
data ChildInstance = ChildInstance { ciComponent :: Text -- ^ Name of parameter to be used as component reference
                                   } deriving (Show)

-- | Definition of a multiple child instances spec.
data MultiInstantiate = MultiInstantiate { miComponent :: Text -- ^ Name of parameter to be used as component reference
                                         , miNumber    :: Text -- ^ Number of instances
                                         } deriving (Show)

-- | Definition of an event connection.
data EventConnection = EventConnection { evcFrom :: Text              -- ^ Variable containing link to source component
                                       , evcTo   :: Text              -- ^ Variable containing link to target component
                                       , evcSourcePort :: Text        -- ^ Source port
                                       , evcTargetPort :: Text        -- ^ Target port
                                       , evcReceiver :: Text          -- ^ Event receiver component (Overrides "to")
                                       , evcReceiverContainer :: Text -- ^ Event receiver container
                                       } deriving (Show)

-- | Definition of a component reference variable assigned a single component.
data With = With { wthInstance :: Text -- ^ Name of link parameter or path to instance
                 , wthAs       :: Text -- ^ Variable name
                 } deriving (Show)
            
-- | Definition of a component reference variable iterating over a list of components.
data ForEach = ForEach { feInstances        :: Text               -- ^ Name of link parameter or path to instance collection
                       , feAs               :: Text               -- ^ Variable name
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
data Record = Record { recQuantity  :: Text -- ^ Name of the parameter containing path to the quantity to be recorded
                     , recTimeScale :: Text -- ^ Name of the parameter containing timeScale value
                     , recScale     :: Text -- ^ Name of the parameter containing a scale value for the recorded quantity
                     , recColor     :: Text -- ^ Name of the parameter specifying the color (for plots)
                     } deriving (Show)

-- | Definition of a data writer.
data DataWriter = DataWriter { dwPath     :: Text -- ^ Name of the parameter containing path of the file to be written to
                             , dwFilename :: Text -- ^ Name of the parameter containing name of the file to be written to
                             } deriving (Show)
                 
-- | Definition of a data display.
data DataDisplay = DataDisplay { ddTitle      :: Text -- ^ Name of the parameter containing title of the plot
                               , ddDataRegion :: Text -- ^ Name of the parameter containing x,y-extents of the plot
                             } deriving (Show)
                 
-- | Definition of a simulation run spec.
data Run = Run { runComponent :: Text -- ^ Name of the parameter containing title of the plot
               , runVariable  :: Text -- ^ Name of the parameter specifying name of the time variable
               , runIncrement :: Text -- ^ Name of the parameter specifying time increment
               , runTotal     :: Text -- ^ Name of the parameter specifying total time of the simulation
               } deriving (Show)

-- | Definition of a simulation spec container.
data Simulation = Simulation { simRecorders    :: [Record]      -- ^ Quantities to be recorded
                             , simDataWriters  :: [DataWriter]  -- ^ File data writers
                             , simDataDisplays :: [DataDisplay] -- ^ Data plotters
                             , simRun          :: [Run]         -- ^ Run specs
                             } deriving (Show)
                 
-- | Definition of a component type.
data ComponentType = ComponentType { compTypeName              :: Text                 -- ^ Name of the component type
                                   , compTypeExtends           :: Text                 -- ^ Name of the base component type
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
                                   , compTypeTexts             :: [Txt]                -- ^ Text definitions
                                   , compTypePaths             :: [Path]               -- ^ Path definitions
                                   , compTypeDynamics          :: Maybe Dynamics       -- ^ Dynamics
                                   , compTypeStructure         :: Maybe Structure      -- ^ Structure
                                   , compTypeSimulation        :: Maybe Simulation     -- ^ Simulation specs
                                   } deriving (Show)

-- | Definition of a container.
data Component = Component { compId         :: Text          -- ^ Component id
                           , compName       :: Text          -- ^ Component name
                           , compType       :: Text          -- ^ Component type
                           , compExtends    :: Text          -- ^ Name of base component
                           , compParameters :: Map Text Text -- ^ Parameters
                           , compChildren   :: [Component]   -- ^ Child components
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
