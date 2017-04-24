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
  )where

import Text.XML.HXT.Core hiding (xread)
import Language.NeuroML.LEMS.Parser.ParseTree

import qualified Data.Map.Strict as M

import Text.XML.HXT.Parser.XmlParsec (xread)
import Text.XML.HXT.Arrow.XmlState
import Data.Tree.NTree.TypeDefs

import Control.Monad as Monad

import Data.Maybe
import Data.Functor

import System.Directory

lemsTopLevelTags = ["Dimension", "Unit", "Assertion",
                    "Include", "Constant",
                    "ComponentType", "Component", "Target"]
  
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


hasNames []         = hasName ""
hasNames (tag:tags) = (hasName tag) `orElse` (hasNames tags)

atDeepTag tag = deep (isElem >>> hasName tag)
atTag tag =  (isElem >>> hasName tag)

notAtTags tags =  (isElem >>> neg (hasNames tags))

strToInt i s = let l = reads s :: [(Int, String)]
             in if length l == 0 then
                  i
                else
                  (fst . head) l

strToDouble d s = let l = reads s :: [(Double, String)]
                  in if length l == 0 then
                       d
                     else
                       (fst . head) l

childListA parseFn = listA (getChildren >>> parseFn)
childA parseFn = getChildren >>> parseFn


getAttrMap skipNames trees = getAttrMap2 M.empty trees
  where getAttrMap2 map []     = map
        getAttrMap2 map (t:ts) = let (name, value) = (getAttr t)
                                 in if name `elem` skipNames
                                    then
                                      getAttrMap2 map ts
                                    else
                                      getAttrMap2 (M.insert name value map) ts
          where getAttr (NTree (XAttr name) children) = (cleanString name, getAttrValue (head children))
                getAttrValue (NTree (XText value) _) = cleanString value
                cleanString name = filter (\c -> c /= '"') $ show name
          
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
    returnA -< (Dimension name
                (strToInt 0 m)
                (strToInt 0 l)
                (strToInt 0 t)
                (strToInt 0 i)
                (strToInt 0 k)
                (strToInt 0 n)
                (strToInt 0 j))


parseUnit = atTag "Unit" >>>
  proc unit -> do
    name      <- getAttrValue "name"      -< unit
    symbol    <- getAttrValue "symbol"    -< unit
    dimension <- getAttrValue "dimension" -< unit
    power     <- getAttrValue "power"     -< unit
    scale     <- getAttrValue "scale"     -< unit
    offset    <- getAttrValue "offset"    -< unit
    returnA -< Unit name symbol dimension (strToInt 0 power) (strToDouble 0.0 scale) (strToDouble 0.0 offset)
       
parseAssertion = atTag "Assertion" >>>
  proc assertion -> do
    dimension <- getAttrValue "dimension" -< assertion
    matches   <- getAttrValue "matches"   -< assertion
    returnA -< Assertion dimension matches
       
parseConstant = atTag "Constant" >>>
  proc constant -> do
    name      <- getAttrValue "name"      -< constant
    matches   <- getAttrValue "matches"   -< constant
    value     <- getAttrValue "value"     -< constant
    dimension <- getAttrValue "dimension" -< constant
    returnA -< Constant name matches value dimension

parseInclude = atTag "Include" >>>
  proc include -> do
    file <- getAttrValue "file" -< include
    returnA -< Include file

parseParameter = atTag "Parameter" >>>
  proc parameter -> do
    name      <- getAttrValue "name"      -< parameter
    dimension <- getAttrValue "dimension" -< parameter
    returnA -< Parameter name dimension
    
parseFixed = atTag "Fixed" >>>
  proc fixed -> do
    name   <- getAttrValue "name"      -< fixed
    value  <- getAttrValue "value" -< fixed
    returnA -< Fixed name value
    
parseDerivedParameter = atTag "DerivedParameter" >>>
  proc derivedParameter -> do
    name      <- getAttrValue "name"      -< derivedParameter
    dimension <- getAttrValue "dimension" -< derivedParameter
    select    <- getAttrValue "select"    -< derivedParameter
    value     <- getAttrValue "value"     -< derivedParameter
    returnA -< DerivedParameter name dimension select value
    
parseComponentReference = atTag "ComponentReference" >>>
  proc componentReference -> do
    name  <- getAttrValue "name" -< componentReference
    ctype <- getAttrValue "type" -< componentReference
    returnA -< ComponentReference name ctype
    
parseLink = atTag "Link" >>>
  proc link -> do
    name <- getAttrValue "name" -< link
    ctype <- getAttrValue "type" -< link
    returnA -< Link name ctype
    
parseExposure = atTag "Exposure" >>>
  proc exposure -> do
    name      <- getAttrValue "name"      -< exposure
    dimension <- getAttrValue "dimension" -< exposure
    returnA -< Exposure name dimension

parseRequirement = atTag "Requirement" >>>
  proc requirement -> do
    name      <- getAttrValue "name"      -< requirement
    dimension <- getAttrValue "dimension" -< requirement
    returnA -< Requirement name dimension

parseChild = atTag "Child" >>>
  proc child -> do
    name  <- getAttrValue "name" -< child
    ctype <- getAttrValue "type" -< child
    returnA -< Child name ctype

parseChildren = atTag "Children" >>>
  proc children -> do
    name  <- getAttrValue "name" -< children
    ctype <- getAttrValue "type" -< children
    returnA -< Children name ctype

parseEventPort = atTag "EventPort" >>>
  proc eventPort -> do
    name      <- getAttrValue "name"      -< eventPort
    dimension <- getAttrValue "dimension" -< eventPort
    returnA -< EventPort name dimension

parseText = atTag "Text" >>>
  proc text -> do
    name <- getAttrValue "name" -< text
    returnA -< Text name
    
parsePath = atTag "Path" >>>
  proc path -> do
    name <- getAttrValue "name" -< path
    returnA -< Path name
    
parseStateVariable = atTag "StateVariable" >>>
  proc stateVariable -> do
    name      <- getAttrValue "name"      -< stateVariable
    exposure  <- getAttrValue "exposure"  -< stateVariable
    dimension <- getAttrValue "dimension" -< stateVariable
    returnA -< StateVariable name exposure dimension

parseDerivedVariable = atTag "DerivedVariable" >>>
  proc derivedVariable -> do
    name      <- getAttrValue "name"      -< derivedVariable
    exposure  <- getAttrValue "exposure"  -< derivedVariable
    dimension <- getAttrValue "dimension" -< derivedVariable
    value     <- getAttrValue "value"     -< derivedVariable
    select    <- getAttrValue "select"    -< derivedVariable
    reduce    <- getAttrValue "reduce"    -< derivedVariable
    required  <- getAttrValue "required"  -< derivedVariable
    returnA -< DerivedVariable name exposure dimension value select reduce required

parseTimeDerivative = atTag "TimeDerivative" >>>
  proc timeDerivative -> do
    variable <- getAttrValue "variable" -< timeDerivative
    value    <- getAttrValue "value"    -< timeDerivative
    returnA -< TimeDerivative variable value

parseStateAssignment = atTag "StateAssignment" >>>
  proc stateAssignment -> do
    variable <- getAttrValue "variable" -< stateAssignment
    value    <- getAttrValue "value"    -< stateAssignment
    returnA -< StateAssignment variable value

parseEventOut = atTag "EventOut" >>>
  proc eventOut -> do
    port <- getAttrValue "port" -< eventOut
    returnA -< EventOut port
    
parseTransition = atTag "Transition" >>>
  proc transition -> do
    regime <- getAttrValue "regime" -< transition
    returnA -< Transition regime
    
parseEventAction = parseStateAssignment `orElse` parseEventOut `orElse` parseTransition

parseOnStart = atTag "OnStart" >>>
  proc onStart -> do
    eventActions <- childListA parseEventAction -< onStart
    returnA -< OnStart eventActions

parseOnEntry = atTag "OnEntry" >>>
  proc onEntry -> do
    eventActions <- childListA parseEventAction -< onEntry
    returnA -< OnEntry eventActions

parseOnCondition = atTag "OnCondition" >>>
  proc onCondition -> do
    test         <- getAttrValue "test"         -< onCondition
    eventActions <- childListA parseEventAction -< onCondition
    returnA -< OnCondition test eventActions

parseOnEvent = atTag "OnEvent" >>>
  proc onEvent -> do
    port         <- getAttrValue "port"         -< onEvent
    eventActions <- childListA parseEventAction -< onEvent
    returnA -< OnEvent port eventActions

parseEventHandler = parseOnStart `orElse` parseOnCondition `orElse` parseOnEvent `orElse` parseOnEntry

parseRegime = atTag "Regime" >>>
  proc regime -> do
    name             <- getAttrValue "name"             -< regime
    initial          <- getAttrValue "initial"          -< regime
    stateVariables   <- childListA parseStateVariable   -< regime
    stateVariables   <- childListA parseStateVariable   -< regime
    timeDerivatives  <- childListA parseTimeDerivative  -< regime
    derivedVariables <- childListA parseDerivedVariable -< regime
    eventHandlers    <- childListA parseEventHandler    -< regime
    returnA -< Regime name initial stateVariables timeDerivatives derivedVariables eventHandlers

parseKineticScheme = atTag "KineticScheme" >>>
  proc ks -> do
    name        <- getAttrValue "name"          -< ks
    nodes       <- getAttrValue "nodes"         -< ks
    stateVar    <- getAttrValue "stateVariable" -< ks
    edges       <- getAttrValue "edges"         -< ks
    edgeSource  <- getAttrValue "edgeSource"    -< ks
    edgeTarget  <- getAttrValue "edgeTarget"    -< ks
    forwardRate <- getAttrValue "forwardRate"   -< ks
    reverseRate <- getAttrValue "reverseRate"   -< ks
    returnA -< KineticScheme name nodes stateVar edges edgeSource edgeTarget forwardRate reverseRate

parseDynamics = atTag "Dynamics" >>>
  proc dynamics -> do
    stateVariables   <- childListA parseStateVariable   -< dynamics
    stateVariables   <- childListA parseStateVariable   -< dynamics
    timeDerivatives  <- childListA parseTimeDerivative  -< dynamics
    derivedVariables <- childListA parseDerivedVariable -< dynamics
    eventHandlers    <- childListA parseEventHandler    -< dynamics
    regimes          <- childListA parseRegime          -< dynamics
    kineticSchemes   <- childListA parseKineticScheme   -< dynamics
    returnA -< Just $ Dynamics stateVariables timeDerivatives derivedVariables eventHandlers regimes kineticSchemes

parseChildInstance = atTag "ChildInstance" >>>
  proc childInstance -> do
    component <- getAttrValue "component" -< childInstance
    returnA -< ChildInstance component
    
parseMultiInstantiate = atTag "MultiInstantiate" >>>
  proc multiInstantiate -> do
    component <- getAttrValue "component" -< multiInstantiate
    number    <- getAttrValue "number" -< multiInstantiate
    returnA -< MultiInstantiate component number
    
parseEventConnection = atTag "EventConnection" >>>
  proc eventConnection -> do
    from              <- getAttrValue "from"              -< eventConnection
    to                <- getAttrValue "to"                -< eventConnection
    sourcePort        <- getAttrValue "sourcePort"        -< eventConnection
    targetPort        <- getAttrValue "targetPort"        -< eventConnection
    receiver          <- getAttrValue "receiver"          -< eventConnection
    receiverContainer <- getAttrValue "receiverContainer" -< eventConnection
    returnA -< EventConnection from to sourcePort targetPort receiver receiverContainer

parseWith = atTag "With" >>>
  proc with -> do
    instance_ <- getAttrValue "instance" -< with
    as        <- getAttrValue "as"       -< with
    returnA -< With instance_ as

parseForEach = atTag "ForEach" >>>
  proc forEach -> do
    instances        <- getAttrValue "instances"        -< forEach
    as               <- getAttrValue "as"               -< forEach
    forEaches        <- childListA parseForEach         -< forEach
    eventConnections <- childListA parseEventConnection -< forEach
    returnA -< ForEach instances as forEaches eventConnections

parseStructure = atTag "Structure" >>>
  proc structure -> do
    childInstances   <- childListA parseChildInstance    -< structure
    multiInstances   <- childListA parseMultiInstantiate -< structure
    eventConnections <- childListA parseEventConnection  -< structure
    withs            <- childListA parseWith             -< structure
    forEaches        <- childListA parseForEach          -< structure
    returnA -< Just $ Structure childInstances multiInstances eventConnections withs forEaches

parseRecord = atTag "Record" >>>
  proc record -> do
    quantity  <- getAttrValue "quantity"  -< record
    timeScale <- getAttrValue "timeScale" -< record
    scale     <- getAttrValue "scale"     -< record
    color     <- getAttrValue "color"     -< record
    returnA -< Record quantity timeScale scale color

parseDataWriter = atTag "DataWriter" >>>
  proc dataWriter -> do
    path     <- getAttrValue "path"     -< dataWriter
    fileName <- getAttrValue "fileName" -< dataWriter
    returnA -< DataWriter path fileName

parseDataDisplay = atTag "DataDisplay" >>>
  proc dataDisplay -> do
    title      <- getAttrValue "title"      -< dataDisplay
    dataRegion <- getAttrValue "dataRegion" -< dataDisplay
    returnA -< DataDisplay title dataRegion

parseRun = atTag "Run" >>>
  proc run -> do
    component <- getAttrValue "component" -< run
    variable  <- getAttrValue "variable"  -< run
    increment <- getAttrValue "increment" -< run
    total     <- getAttrValue "total"     -< run
    returnA -< Run component variable increment total

parseSimulation = atTag "Simulation" >>>
  proc simulation -> do
    recorders    <- childListA parseRecord    -< simulation
    dataWriters  <- childListA parseDataWriter    -< simulation
    dataDisplays <- childListA parseDataDisplay    -< simulation
    runs         <- childListA parseRun    -< simulation
    returnA -< Just $ Simulation recorders dataWriters dataDisplays runs
    
parseComponentType = atTag "ComponentType" >>>
  proc compType -> do
    name              <- getAttrValue "name"                          -< compType
    extends           <- getAttrValue "extends"                       -< compType
    parameters        <- childListA parseParameter                    -< compType
    fixedParameters   <- childListA parseFixed                        -< compType
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
    returnA -< ComponentType name extends parameters fixedParameters derivedParameters compRefs links exposures requirements childDefs childrenDefs eventPorts texts paths dynamics structure simulation

parseComponentExplicit = atTag "Component" >>>
  proc comp -> do
    id       <- getAttrValue "id"      -< comp
    name     <- getAttrValue "name"    -< comp
    extends  <- getAttrValue "extends" -< comp
    ctype    <- getAttrValue "type"    -< comp
    attrList <- listA getAttrl         -< comp
    children <- childListA (parseComponentImplicit lemsTags) -< comp
    returnA -< Component id name ctype extends (getAttrMap ["id", "name", "extends", "type"] attrList) children

parseComponentImplicit skipTags = notAtTags skipTags >>>
  proc comp -> do
    id       <- getAttrValue "id"      -< comp
    name     <- getAttrValue "name"    -< comp
    extends  <- getAttrValue "extends" -< comp
    ctype    <- getName                -< comp
    attrList <- listA getAttrl    -< comp
    children <- childListA (parseComponentImplicit lemsTags) -< comp
    returnA -< Component id name ctype extends (getAttrMap ["id", "name", "extends", "type"] attrList) children

parseComponent = parseComponentExplicit `orElse` (parseComponentImplicit lemsTopLevelTags)

parseTarget = atTag "Target" >>>
  proc tgt -> do
    component  <- getAttrValue "component"  -< tgt
    reportFile <- getAttrValue "reportFile" -< tgt
    timesFile  <- getAttrValue "timesFile"  -< tgt
    returnA -< Just $ Target component reportFile timesFile
       
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


parseXML xmlText = readString [ withValidate no
                              , withRemoveWS yes  -- throw away formating WS
                              ] xmlText

emptyModel = Lems [] [] [] [] [] [] [] Nothing

concatModels []     = emptyModel
concatModels (m:ms) = let msc = concatModels ms
                          d1 = lemsDimensions m
                          u1 = lemsUnits m
                          a1 = lemsAssertions m
                          cns1 = lemsConstants m
                          ct1 = lemsCompTypes m
                          cmp1 = lemsComponents m
                          t1 = lemsTarget m
                          d2 = lemsDimensions msc
                          u2 = lemsUnits msc
                          a2 = lemsAssertions msc
                          cns2 = lemsConstants msc
                          ct2 = lemsCompTypes msc
                          cmp2 = lemsComponents msc
                          t2 = lemsTarget msc
                          tnew = if isNothing t1 then t2 else t1
                      in Lems [] (d1 ++ d2) (u1 ++ u2) (a1 ++ a2) (cns1 ++ cns2) (ct1 ++ ct2) (cmp1 ++ cmp2) tnew

-- | Parses a string containing an LEMS formatted XML into a parse tree. Does not process include directives.
parseLemsXML :: String           -- ^ XML text
             -> IO (Maybe Lems)
parseLemsXML xmlText = do
  parseTrees <- runX (parseXML xmlText >>> parseLems)
  let parseTree = listToMaybe parseTrees
  return parseTree

findAndParseLemsXMLIncludeFile :: [String] -> String -> IO (Maybe Lems)
findAndParseLemsXMLIncludeFile includeDirs xmlFile = do
  path <- findFile includeDirs xmlFile
  model <- parseLemsXMLFile includeDirs $ fromJust path
  return model

-- | Takes a list of directories to search for included files and XML file name, parses the file and any included files, and returns a parse tree.
parseLemsXMLFile :: [String]        -- ^ List of directories to search for included files
                 -> String          -- ^ Path to the file to be parsed
                 -> IO (Maybe Lems)
parseLemsXMLFile includeDirs xmlFile = do
  contents <- readFile xmlFile
  maybeModel <- parseLemsXML contents
  Monad.when (isNothing maybeModel) $ error "Unable to parse XML file"
  let model = fromJust maybeModel
      includedFiles = map includeFile $ lemsIncludes $ model
  includedModels <- mapM (findAndParseLemsXMLIncludeFile includeDirs) includedFiles
  return $ Just $ concatModels $ model:(catMaybes includedModels)

  
-----------------------------------------------------------------------------------------------------------

test file = do
  --contents <- readFile file
  --model <- parseLemsXML contents
  model <- parseLemsXMLFile includeDirs file
  let ctype = fromMaybe Nothing $ listToMaybe <$> filter (\ct -> compTypeName ct == "KSState") <$> lemsCompTypes <$> model
  let comp = fromMaybe Nothing $ listToMaybe <$> filter (\c -> compId c == "na1") <$> lemsComponents <$> model
  putStrLn $ show model
  putStrLn ""
  putStrLn ""
  putStrLn ""
  putStrLn $ show ctype
  putStrLn ""
  putStrLn $ show $ fmap compTypeSimulation $  ctype
  putStrLn ""
  putStrLn ""
  putStrLn ""
  putStrLn $ show $ comp
  putStrLn ""
  putStrLn ""
  putStrLn ""
  putStrLn $ show $ map compId <$> lemsComponents <$> model

testLems lemsFile = test $ "/home/gautham/work/NeuroML/LEMS/examples/" ++ lemsFile

test1 = testLems "example4.xml"

test2 = do
    contents <- readFile "/home/gautham/work/NeuroML/LEMS/examples/example1.xml"
    putStrLn $ show $ head (xread "<comp a=\"1\" b = \"2\"/>") 
    putStrLn $ show $ head (xread "<comp a=\"1\" b = \"2\"/>") 

ex4 = "/home/gautham/work/NeuroML/LEMS/examples/example4.xml"

includeDirs = ["/home/gautham/work/NeuroML/LEMS/examples"]
