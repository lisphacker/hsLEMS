{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-|
Module      : Language.NeuroML.LEMS.Parser.XMLParser
Description : Parsed LEMS model
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

LEMS model after being parsed from XML.
-}
module Language.NeuroML.LEMS.Parser.XMLParser
  ( parseLemsXML
  , parseLemsXMLFile
  ) where

import Protolude hiding (orElse)

import Text.XML.HXT.Core hiding (xread)
import Language.NeuroML.LEMS.Parser.ParseTree
import Language.NeuroML.LEMS.Errors
import qualified Data.Map.Strict as M

import Data.Tree.NTree.TypeDefs

import Data.Maybe
import Data.String

import System.Directory

import Data.Text (pack, unpack)

lemsTopLevelTags :: IsString a => [a]
lemsTopLevelTags = ["Dimension", "Unit", "Assertion",
                    "Include", "Constant",
                    "ComponentType", "Component", "Target"]
  
lemsTags :: IsString a => [a]
lemsTags = ["Lems",
            "Dimension", "Unit", "Assertion",
            "Include", "Constant",
            "ComponentType", "Component", "Target",
            "Parameter", "Fixed", "DerivedParameter", "Exposure",
            "Requirement",
            "Child", "Children", "Attachments", 
            "Link", "ComponentReference",
            "EventPort",

            "Text", "Path",
            
            "Dynamics",
            "StateVariable", "TimeDerivative",
            "DerivedVariable",
            "OnStart", "OnCondition", "OnEvent",
            "StateAssignment", "EventOut",
            "KineticScheme",
            "Regime",
            "OnEntry", "Transition",
            
            "Structure",
            "ChildInstance", "MultiInstantiate",
            "ForEach", "With",
            "EventConnection",

            "Simulation",
            "Record", "DataDisplay", "DataWriter", "Run"]


hasNames :: ArrowXml a => [String] -> a XmlTree XmlTree
hasNames []         = hasName ""
hasNames (tag:tags) = (hasName tag) `orElse` (hasNames tags)

atDeepTag :: ArrowXml a => String -> a (NTree XNode) XmlTree
atDeepTag tag = deep (isElem >>> hasName tag)
atTag :: ArrowXml cat => String -> cat XmlTree XmlTree
atTag tag =  (isElem >>> hasName tag)

notAtTags :: ArrowXml cat => [String] -> cat XmlTree XmlTree
notAtTags tags =  (isElem >>> neg (hasNames tags))
strToInt :: Int -> String -> Int
strToInt i s = let l = reads s :: [(Int, String)]
               in case l of
                    []     -> i
                    (x:_) -> fst x
strToDouble :: Double -> String -> Double
strToDouble d s = let l = reads s :: [(Double, String)]
                  in case l of
                       []     -> d
                       (x:_) -> fst x

childListA :: (ArrowTree a, Tree t) => a (t b) c -> a (t b) [c]
childListA parseFn = listA (getChildren >>> parseFn)
childA :: (ArrowTree cat, Tree t) => cat (t b) c -> cat (t b) c
childA parseFn = getChildren >>> parseFn


getAttrMap :: Foldable t => t [Char] -> [NTree XNode] -> Map Text Text
getAttrMap skipNames trees = getAttrMap' M.empty trees
  where getAttrMap' map []     = map
        getAttrMap' map (t:ts) = let (name, value) = (getAttr t)
                                 in if name `elem` skipNames
                                    then
                                      getAttrMap' map ts
                                    else
                                      getAttrMap' (M.insert (pack name) (pack value) map) ts
          where getAttr (NTree (XAttr name) children) = (cleanString name, getAttrValue (fromJust $ head children))
                getAttr _                             = ("", "")
                
                getAttrValue (NTree (XText value) _) = cleanString value
                getAttrValue _                       = ""
                
                cleanString name = filter (\c -> c /= '"') $ show name
          
parseDimension :: ArrowXml cat => cat XmlTree Dimension
parseDimension = atTag "Dimension" >>>
  proc dim -> do
    name <- getAttrValue "name" -< dim
    m    <- getAttrValue "m"    -< dim
    l    <- getAttrValue "l"    -< dim
    t    <- getAttrValue "t"    -< dim
    i    <- getAttrValue "i"    -< dim
    k    <- getAttrValue "k"    -< dim
    n    <- getAttrValue "n"    -< dim
    j    <- getAttrValue "j"    -< dim
    returnA -< (Dimension (pack name)
                (strToInt 0 m)
                (strToInt 0 l)
                (strToInt 0 t)
                (strToInt 0 i)
                (strToInt 0 k)
                (strToInt 0 n)
                (strToInt 0 j))


parseUnit :: ArrowXml cat => cat XmlTree Unit
parseUnit = atTag "Unit" >>>
  proc unit -> do
    name      <- pack ^<< getAttrValue "name"      -< unit
    symbol    <- pack ^<< getAttrValue "symbol"    -< unit
    dimension <- pack ^<< getAttrValue "dimension" -< unit
    power     <- getAttrValue "power"              -< unit
    scale     <- getAttrValue "scale"              -< unit
    offset    <- getAttrValue "offset"             -< unit
    returnA -< Unit name symbol dimension (strToInt 0 power) (strToDouble 0.0 scale) (strToDouble 0.0 offset)
       
parseAssertion :: ArrowXml cat => cat XmlTree Assertion
parseAssertion = atTag "Assertion" >>>
  proc assertion -> do
    dimension <- pack ^<< getAttrValue "dimension" -< assertion
    matches   <- pack ^<< getAttrValue "matches"   -< assertion
    returnA -< Assertion dimension matches
       
parseConstant :: ArrowXml cat => cat XmlTree Constant
parseConstant = atTag "Constant" >>>
  proc constant -> do
    name      <- pack ^<< getAttrValue "name"      -< constant
    dimension <- pack ^<< getAttrValue "dimension" -< constant
    value     <- pack ^<< getAttrValue "value"     -< constant
    returnA -< Constant name dimension value 

parseInclude :: ArrowXml cat => cat XmlTree Include
parseInclude = atTag "Include" >>>
  proc include -> do
    file <- pack ^<< getAttrValue "file" -< include
    returnA -< Include file

parseParameter :: ArrowXml cat => cat XmlTree Parameter
parseParameter = atTag "Parameter" >>>
  proc parameter -> do
    name      <- pack ^<< getAttrValue "name"      -< parameter
    dimension <- pack ^<< getAttrValue "dimension" -< parameter
    returnA -< Parameter name dimension
    
parseFixed :: ArrowXml cat => cat XmlTree Fixed
parseFixed = atTag "Fixed" >>>
  proc fixed -> do
    name   <- pack ^<< getAttrValue "name"      -< fixed
    value  <- pack ^<< getAttrValue "value" -< fixed
    returnA -< Fixed name value
    
parseDerivedParameter :: ArrowXml cat => cat XmlTree DerivedParameter
parseDerivedParameter = atTag "DerivedParameter" >>>
  proc derivedParameter -> do
    name      <- pack ^<< getAttrValue "name"      -< derivedParameter
    dimension <- pack ^<< getAttrValue "dimension" -< derivedParameter
    select    <- pack ^<< getAttrValue "select"    -< derivedParameter
    value     <- pack ^<< getAttrValue "value"     -< derivedParameter
    returnA -< DerivedParameter name dimension select value
    
parseComponentReference :: ArrowXml cat => cat XmlTree ComponentReference
parseComponentReference = atTag "ComponentReference" >>>
  proc componentReference -> do
    name  <- pack ^<< getAttrValue "name" -< componentReference
    ctype <- pack ^<< getAttrValue "type" -< componentReference
    returnA -< ComponentReference name ctype
    
parseLink :: ArrowXml cat => cat XmlTree Link
parseLink = atTag "Link" >>>
  proc link -> do
    name  <- pack ^<< getAttrValue "name" -< link
    ctype <- pack ^<< getAttrValue "type" -< link
    returnA -< Link name ctype
    
parseExposure :: ArrowXml cat => cat XmlTree Exposure
parseExposure = atTag "Exposure" >>>
  proc exposure -> do
    name      <- pack ^<< getAttrValue "name"      -< exposure
    dimension <- pack ^<< getAttrValue "dimension" -< exposure
    returnA -< Exposure name dimension

parseRequirement :: ArrowXml cat => cat XmlTree Requirement
parseRequirement = atTag "Requirement" >>>
  proc requirement -> do
    name      <- pack ^<< getAttrValue "name"      -< requirement
    dimension <- pack ^<< getAttrValue "dimension" -< requirement
    returnA -< Requirement name dimension

parseChild :: ArrowXml cat => cat XmlTree Child
parseChild = atTag "Child" >>>
  proc child -> do
    name  <- pack ^<< getAttrValue "name" -< child
    ctype <- pack ^<< getAttrValue "type" -< child
    returnA -< Child name ctype

parseChildren :: ArrowXml cat => cat XmlTree Children
parseChildren = atTag "Children" >>>
  proc children -> do
    name  <- pack ^<< getAttrValue "name" -< children
    ctype <- pack ^<< getAttrValue "type" -< children
    returnA -< Children name ctype

parseEventPort :: ArrowXml cat => cat XmlTree EventPort
parseEventPort = atTag "EventPort" >>>
  proc eventPort -> do
    name      <- pack ^<< getAttrValue "name"      -< eventPort
    dimension <- pack ^<< getAttrValue "dimension" -< eventPort
    returnA -< EventPort name dimension

parseText :: ArrowXml cat => cat XmlTree Txt
parseText = atTag "Text" >>>
  proc text -> do
    name <- pack ^<< getAttrValue "name" -< text
    returnA -< Txt name
    
parsePath :: ArrowXml cat => cat XmlTree Path
parsePath = atTag "Path" >>>
  proc path -> do
    name <- pack ^<< getAttrValue "name" -< path
    returnA -< Path name
    
parseStateVariable :: ArrowXml cat => cat XmlTree StateVariable
parseStateVariable = atTag "StateVariable" >>>
  proc stateVariable -> do
    name      <- pack ^<< getAttrValue "name"      -< stateVariable
    exposure  <- pack ^<< getAttrValue "exposure"  -< stateVariable
    dimension <- pack ^<< getAttrValue "dimension" -< stateVariable
    returnA -< StateVariable name exposure dimension

parseDerivedVariable :: ArrowXml cat => cat XmlTree DerivedVariable
parseDerivedVariable = atTag "DerivedVariable" >>>
  proc derivedVariable -> do
    name      <- pack ^<< getAttrValue "name"      -< derivedVariable
    exposure  <- pack ^<< getAttrValue "exposure"  -< derivedVariable
    dimension <- pack ^<< getAttrValue "dimension" -< derivedVariable
    value     <- pack ^<< getAttrValue "value"     -< derivedVariable
    select    <- pack ^<< getAttrValue "select"    -< derivedVariable
    reduce    <- pack ^<< getAttrValue "reduce"    -< derivedVariable
    required  <- pack ^<< getAttrValue "required"  -< derivedVariable
    returnA -< DerivedVariable name exposure dimension value select reduce required

parseTimeDerivative :: ArrowXml cat => cat XmlTree TimeDerivative
parseTimeDerivative = atTag "TimeDerivative" >>>
  proc timeDerivative -> do
    variable <- pack ^<< getAttrValue "variable" -< timeDerivative
    value    <- pack ^<< getAttrValue "value"    -< timeDerivative
    returnA -< TimeDerivative variable value

parseStateAssignment :: ArrowXml cat => cat XmlTree Action
parseStateAssignment = atTag "StateAssignment" >>>
  proc stateAssignment -> do
    variable <- pack ^<< getAttrValue "variable" -< stateAssignment
    value    <- pack ^<< getAttrValue "value"    -< stateAssignment
    returnA -< StateAssignment variable value

parseEventOut :: ArrowXml cat => cat XmlTree Action
parseEventOut = atTag "EventOut" >>>
  proc eventOut -> do
    port <- pack ^<< getAttrValue "port" -< eventOut
    returnA -< EventOut port
    
parseTransition :: ArrowXml cat => cat XmlTree Action
parseTransition = atTag "Transition" >>>
  proc transition -> do
    regime <- pack ^<< getAttrValue "regime" -< transition
    returnA -< Transition regime
    
parseEventAction :: ArrowXml a => a XmlTree Action
parseEventAction = parseStateAssignment `orElse` parseEventOut `orElse` parseTransition

parseOnStart :: ArrowXml cat => cat XmlTree EventHandler
parseOnStart = atTag "OnStart" >>>
  proc onStart -> do
    eventActions <- childListA parseEventAction -< onStart
    returnA -< OnStart eventActions

parseOnEntry :: ArrowXml cat => cat XmlTree EventHandler
parseOnEntry = atTag "OnEntry" >>>
  proc onEntry -> do
    eventActions <- childListA parseEventAction -< onEntry
    returnA -< OnEntry eventActions

parseOnCondition :: ArrowXml cat => cat XmlTree EventHandler
parseOnCondition = atTag "OnCondition" >>>
  proc onCondition -> do
    test         <- pack ^<< getAttrValue "test" -< onCondition
    eventActions <- childListA parseEventAction  -< onCondition
    returnA -< OnCondition test eventActions

parseOnEvent :: ArrowXml cat => cat XmlTree EventHandler
parseOnEvent = atTag "OnEvent" >>>
  proc onEvent -> do
    port         <- pack ^<< getAttrValue "port" -< onEvent
    eventActions <- childListA parseEventAction  -< onEvent
    returnA -< OnEvent port eventActions

parseEventHandler :: ArrowXml a => a XmlTree EventHandler
parseEventHandler = parseOnStart `orElse` parseOnCondition `orElse` parseOnEvent `orElse` parseOnEntry

parseRegime :: ArrowXml cat => cat XmlTree Regime
parseRegime = atTag "Regime" >>>
  proc regime -> do
    name             <- pack ^<< getAttrValue "name"    -< regime
    initial          <- pack ^<< getAttrValue "initial" -< regime
    --stateVariables   <- childListA parseStateVariable   -< regime
    stateVariables   <- childListA parseStateVariable   -< regime
    timeDerivatives  <- childListA parseTimeDerivative  -< regime
    derivedVariables <- childListA parseDerivedVariable -< regime
    eventHandlers    <- childListA parseEventHandler    -< regime
    returnA -< Regime name initial stateVariables timeDerivatives derivedVariables eventHandlers

parseKineticScheme :: ArrowXml cat => cat XmlTree KineticScheme
parseKineticScheme = atTag "KineticScheme" >>>
  proc ks -> do
    name        <- pack ^<< getAttrValue "name"          -< ks
    nodes       <- pack ^<< getAttrValue "nodes"         -< ks
    stateVar    <- pack ^<< getAttrValue "stateVariable" -< ks
    edges       <- pack ^<< getAttrValue "edges"         -< ks
    edgeSource  <- pack ^<< getAttrValue "edgeSource"    -< ks
    edgeTarget  <- pack ^<< getAttrValue "edgeTarget"    -< ks
    forwardRate <- pack ^<< getAttrValue "forwardRate"   -< ks
    reverseRate <- pack ^<< getAttrValue "reverseRate"   -< ks
    returnA -< KineticScheme name nodes stateVar edges edgeSource edgeTarget forwardRate reverseRate

parseDynamics :: ArrowXml cat => cat XmlTree (Maybe Dynamics)
parseDynamics = atTag "Dynamics" >>>
  proc dynamics -> do
    --stateVariables   <- childListA parseStateVariable   -< dynamics
    stateVariables   <- childListA parseStateVariable   -< dynamics
    timeDerivatives  <- childListA parseTimeDerivative  -< dynamics
    derivedVariables <- childListA parseDerivedVariable -< dynamics
    eventHandlers    <- childListA parseEventHandler    -< dynamics
    regimes          <- childListA parseRegime          -< dynamics
    kineticSchemes   <- childListA parseKineticScheme   -< dynamics
    returnA -< Just $ Dynamics stateVariables timeDerivatives derivedVariables eventHandlers regimes kineticSchemes

parseChildInstance :: ArrowXml cat => cat XmlTree ChildInstance
parseChildInstance = atTag "ChildInstance" >>>
  proc childInstance -> do
    component <- pack ^<< getAttrValue "component" -< childInstance
    returnA -< ChildInstance component
    
parseMultiInstantiate :: ArrowXml cat => cat XmlTree MultiInstantiate
parseMultiInstantiate = atTag "MultiInstantiate" >>>
  proc multiInstantiate -> do
    component <- pack ^<< getAttrValue "component" -< multiInstantiate
    number    <- pack ^<< getAttrValue "number" -< multiInstantiate
    returnA -< MultiInstantiate component number
    
parseEventConnection :: ArrowXml cat => cat XmlTree EventConnection
parseEventConnection = atTag "EventConnection" >>>
  proc eventConnection -> do
    from              <- pack ^<< getAttrValue "from"              -< eventConnection
    to                <- pack ^<< getAttrValue "to"                -< eventConnection
    sourcePort        <- pack ^<< getAttrValue "sourcePort"        -< eventConnection
    targetPort        <- pack ^<< getAttrValue "targetPort"        -< eventConnection
    receiver          <- pack ^<< getAttrValue "receiver"          -< eventConnection
    receiverContainer <- pack ^<< getAttrValue "receiverContainer" -< eventConnection
    returnA -< EventConnection from to sourcePort targetPort receiver receiverContainer

parseWith :: ArrowXml cat => cat XmlTree With
parseWith = atTag "With" >>>
  proc with -> do
    instance_ <- pack ^<< getAttrValue "instance" -< with
    as        <- pack ^<< getAttrValue "as"       -< with
    returnA -< With instance_ as

parseForEach :: ArrowXml cat => cat (NTree XNode) ForEach
parseForEach = atTag "ForEach" >>>
  proc forEach -> do
    instances        <- pack ^<< getAttrValue "instances" -< forEach
    as               <- pack ^<< getAttrValue "as"        -< forEach
    forEaches        <- childListA parseForEach           -< forEach
    eventConnections <- childListA parseEventConnection   -< forEach
    returnA -< ForEach instances as forEaches eventConnections

parseStructure :: ArrowXml cat => cat XmlTree (Maybe Structure)
parseStructure = atTag "Structure" >>>
  proc structure -> do
    childInstances   <- childListA parseChildInstance    -< structure
    multiInstances   <- childListA parseMultiInstantiate -< structure
    eventConnections <- childListA parseEventConnection  -< structure
    withs            <- childListA parseWith             -< structure
    forEaches        <- childListA parseForEach          -< structure
    returnA -< Just $ Structure childInstances multiInstances eventConnections withs forEaches

parseRecord :: ArrowXml cat => cat XmlTree Record
parseRecord = atTag "Record" >>>
  proc record -> do
    quantity  <- pack ^<< getAttrValue "quantity"  -< record
    timeScale <- pack ^<< getAttrValue "timeScale" -< record
    scale     <- pack ^<< getAttrValue "scale"     -< record
    color     <- pack ^<< getAttrValue "color"     -< record
    returnA -< Record quantity timeScale scale color

parseDataWriter :: ArrowXml cat => cat XmlTree DataWriter
parseDataWriter = atTag "DataWriter" >>>
  proc dataWriter -> do
    path     <- pack ^<< getAttrValue "path"     -< dataWriter
    fileName <- pack ^<< getAttrValue "fileName" -< dataWriter
    returnA -< DataWriter path fileName

parseDataDisplay :: ArrowXml cat => cat XmlTree DataDisplay
parseDataDisplay = atTag "DataDisplay" >>>
  proc dataDisplay -> do
    title      <- pack ^<< getAttrValue "title"      -< dataDisplay
    dataRegion <- pack ^<< getAttrValue "dataRegion" -< dataDisplay
    returnA -< DataDisplay title dataRegion

parseRun :: ArrowXml cat => cat XmlTree Run
parseRun = atTag "Run" >>>
  proc run -> do
    component <- pack ^<< getAttrValue "component" -< run
    variable  <- pack ^<< getAttrValue "variable"  -< run
    increment <- pack ^<< getAttrValue "increment" -< run
    total     <- pack ^<< getAttrValue "total"     -< run
    returnA -< Run component variable increment total

parseSimulation :: ArrowXml cat => cat XmlTree (Maybe Simulation)
parseSimulation = atTag "Simulation" >>>
  proc simulation -> do
    recorders    <- childListA parseRecord    -< simulation
    dataWriters  <- childListA parseDataWriter    -< simulation
    dataDisplays <- childListA parseDataDisplay    -< simulation
    runs         <- childListA parseRun    -< simulation
    returnA -< Just $ Simulation recorders dataWriters dataDisplays runs
    
parseComponentType :: ArrowXml cat => cat XmlTree ComponentType
parseComponentType = atTag "ComponentType" >>>
  proc compType -> do
    name              <- pack ^<< getAttrValue "name"                 -< compType
    extends           <- pack ^<< getAttrValue "extends"              -< compType
    parameters        <- childListA parseParameter                    -< compType
    fixedParameters   <- childListA parseFixed                        -< compType
    constants         <- childListA parseConstant                     -< compType
    derivedParameters <- childListA parseDerivedParameter             -< compType
    compRefs          <- childListA parseComponentReference           -< compType
    links             <- childListA parseLink                         -< compType
    exposures         <- childListA parseExposure                     -< compType
    requirements      <- childListA parseRequirement                  -< compType
    childDefs         <- childListA parseChild                        -< compType
    childrenDefs      <- childListA parseChildren                     -< compType
    eventPorts        <- childListA parseEventPort                    -< compType
    texts             <- childListA parseText                         -< compType
    paths             <- childListA parsePath                         -< compType
    dynamics          <- withDefault (childA parseDynamics) Nothing   -< compType
    structure         <- withDefault (childA parseStructure) Nothing  -< compType
    simulation        <- withDefault (childA parseSimulation) Nothing -< compType
    returnA -< ComponentType name extends parameters fixedParameters constants derivedParameters compRefs links exposures requirements childDefs childrenDefs eventPorts texts paths dynamics structure simulation

parseComponentExplicit :: ArrowXml cat => cat XmlTree Component
parseComponentExplicit = atTag "Component" >>>
  proc comp -> do
    id       <- pack ^<< getAttrValue "id"                   -< comp
    name     <- pack ^<< getAttrValue "name"                 -< comp
    extends  <- pack ^<< getAttrValue "extends"              -< comp
    ctype    <- pack ^<< getAttrValue "type"                 -< comp
    attrList <- listA getAttrl                               -< comp
    children <- childListA (parseComponentImplicit lemsTags) -< comp
    returnA -< Component id name ctype extends (getAttrMap ["id", "name", "extends", "type"] attrList) children

parseComponentImplicit :: ArrowXml cat => [String] -> cat (NTree XNode) Component
parseComponentImplicit skipTags = notAtTags skipTags >>>
  proc comp -> do
    id       <- pack ^<< getAttrValue "id"                   -< comp
    name     <- pack ^<< getAttrValue "name"                 -< comp
    extends  <- pack ^<< getAttrValue "extends"              -< comp
    ctype    <- pack ^<< getName                             -< comp
    attrList <- listA getAttrl                               -< comp
    children <- childListA (parseComponentImplicit lemsTags) -< comp
    returnA -< Component id name ctype extends (getAttrMap ["id", "name", "extends", "type"] attrList) children

parseComponent :: ArrowXml a => a XmlTree Component
parseComponent = parseComponentExplicit `orElse` (parseComponentImplicit lemsTopLevelTags)

parseTarget :: ArrowXml cat => cat XmlTree (Maybe Target)
parseTarget = atTag "Target" >>>
  proc tgt -> do
    component  <- pack ^<< getAttrValue "component"  -< tgt
    reportFile <- pack ^<< getAttrValue "reportFile" -< tgt
    timesFile  <- pack ^<< getAttrValue "timesFile"  -< tgt
    returnA -< Just $ Target component reportFile timesFile

parseLems :: ArrowXml cat => cat (NTree XNode) Lems
parseLems = atDeepTag "Lems" >>>
  proc lems -> do
    includes   <- childListA parseInclude       -< lems
    dimensions <- childListA parseDimension     -< lems
    units      <- childListA parseUnit          -< lems
    assertions <- childListA parseAssertion     -< lems
    constants  <- childListA parseConstant      -< lems
    compTypes  <- childListA parseComponentType -< lems
    components <- childListA parseComponent     -< lems
    tgt        <- withDefault (childA parseTarget) Nothing -< lems
    returnA -< Lems includes dimensions units assertions constants compTypes components tgt

parseXML :: String -> IOStateArrow s b XmlTree
parseXML xmlText = readString [ withValidate no
                              , withRemoveWS yes  -- throw away formating WS
                              ] xmlText

emptyModel :: Lems
emptyModel = Lems [] [] [] [] [] [] [] Nothing

concatModels :: [Lems] -> Lems
concatModels []     = emptyModel
concatModels (m:ms) = let msc  = concatModels ms
                          d1   = lemsDimensions m
                          u1   = lemsUnits m
                          a1   = lemsAssertions m
                          cns1 = lemsConstants m
                          ct1  = lemsComponentTypes m
                          cmp1 = lemsComponents m
                          t1   = lemsTarget m
                          d2   = lemsDimensions msc
                          u2   = lemsUnits msc
                          a2   = lemsAssertions msc
                          cns2 = lemsConstants msc
                          ct2  = lemsComponentTypes msc
                          cmp2 = lemsComponents msc
                          t2   = lemsTarget msc
                          tnew = if isNothing t1 then t2 else t1
                      in Lems [] (d1 ++ d2) (u1 ++ u2) (a1 ++ a2) (cns1 ++ cns2) (ct1 ++ ct2) (cmp1 ++ cmp2) tnew


-- | Parses a string containing an LEMS formatted XML into a parse tree. Does not process include directives.
parseLemsXML :: String           -- ^ XML text
             -> IO (Either CompilerError Lems)
parseLemsXML xmlText = do
  parseTrees <- runX (parseXML xmlText >>> parseLems)
  return $ case parseTrees of
             [] -> Left $ InvalidLEMSXML $ pack xmlText
             (pt:_) -> Right pt

findAndParseLemsXMLIncludeFile :: [String] -> String -> IO (Either CompilerError Lems)
findAndParseLemsXMLIncludeFile includeDirs xmlFile = do
  maybePath <- findFile includeDirs xmlFile
  case maybePath of
    Nothing   -> return $ Left $ IncludeFileMissing $ pack xmlFile
    Just path -> parseLemsXMLFile includeDirs path

-- | Takes a list of directories to search for included files and XML file name, parses the file and any included files, and returns a parse tree.
parseLemsXMLFile :: [String]        -- ^ List of directories to search for included files
                 -> String          -- ^ Path to the file to be parsed
                 -> IO (Either CompilerError Lems)
parseLemsXMLFile includeDirs xmlFile = do
  contents <- unpack <$> readFile xmlFile
  eiModel <- parseLemsXML contents
  let includedFiles = map (unpack . includeFile) <$> lemsIncludes <$> eiModel
  includedModels <- parseFiles includedFiles
  return $ case eiModel of
             Left e      -> Left e
             Right model -> case includedModels of
                              Left e       -> Left e
                              Right models -> Right $ concatModels (model:models)
    where parseFiles :: Either CompilerError [String] -> IO (Either CompilerError [Lems])
          parseFiles (Left e) = return $ Left e
          parseFiles (Right []) = return $ Right []
          parseFiles (Right (f:fs)) = do
            maybem <- findAndParseLemsXMLIncludeFile includeDirs f
            maybems <- parseFiles $ Right fs
            return $ case (maybem, maybems) of
              (Right m, Right ms) -> Right (m:ms)
              (Left e,  _       ) -> Left e
              (_,       Left e)   -> Left e
              


-----------------------------------------------------------------------------------------------------------
{-
strout :: String -> IO ()
strout = putStrLn

test :: String -> IO ()
test file = do
  --contents <- readFile file
  --model <- parseLemsXML contents
  model <- parseLemsXMLFile includeDirs file
  let ctype = fromMaybe Nothing $ listToMaybe <$> filter (\ct -> compTypeName ct == "KSState") <$> lemsCompTypes <$> model
  let comp = fromMaybe Nothing $ listToMaybe <$> filter (\c -> compId c == "na1") <$> lemsComponents <$> model
  strout $ show model
  strout ""
  strout ""
  strout ""
  strout $ show ctype
  strout ""
  strout $ show $ fmap compTypeSimulation $  ctype
  strout ""
  strout ""
  strout ""
  strout $ show $ comp
  strout ""
  strout ""
  strout ""
  strout $ show $ map compId <$> lemsComponents <$> model

testLems :: String -> IO ()
testLems lemsFile = test $ "/home/gautham/work/NeuroML/LEMS/examples/" ++ lemsFile

test1 = testLems "example4.xml"

{-
test2 = do
    contents <- readFile "/home/gautham/work/NeuroML/LEMS/examples/example1.xml"
    strout $ show $ head (xread "<comp a=\"1\" b = \"2\"/>") 
    strout $ show $ head (xread "<comp a=\"1\" b = \"2\"/>") 
-}

ex4 = "/home/gautham/work/NeuroML/LEMS/examples/example4.xml" :: String

includeDirs = ["/home/gautham/work/NeuroML/LEMS/examples"] :: [String]
-}
