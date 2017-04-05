{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
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
module Language.NeuroML.LEMS.Parser where

import Text.XML.HXT.Core
import Language.NeuroML.LEMS.Model

atTag tag = deep (isElem >>> hasName tag)

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
    
parseExposure = atTag "Exposure" >>>
  proc exposure -> do
    name      <- getAttrValue "name"      -< exposure
    dimension <- getAttrValue "dimension" -< exposure
    returnA -< Exposure name dimension
    
parseEventPort = atTag "EventPort" >>>
  proc eventPort -> do
    name      <- getAttrValue "name"      -< eventPort
    dimension <- getAttrValue "dimension" -< eventPort
    returnA -< EventPort name dimension

parseStateVariable = atTag "StateVariable" >>>
  proc stateVariable -> do
    name      <- getAttrValue "name"      -< stateVariable
    exposure  <- getAttrValue "exposure"  -< stateVariable
    dimension <- getAttrValue "dimension" -< stateVariable
    returnA -< StateVariable name exposure dimension

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
    
parseEventAction = parseStateAssignment `orElse` parseEventOut

parseOnStart = atTag "OnStart" >>>
  proc onStart -> do
    eventActions <- listA parseEventAction -< onStart
    returnA -< OnStart eventActions

parseOnCondition = atTag "OnCondition" >>>
  proc onCondition -> do
    test <- getAttrValue "test" -< onCondition
    eventActions <- listA parseEventAction -< onCondition
    returnA -< OnCondition test eventActions

parseOnEvent = atTag "OnEvent" >>>
  proc onEvent -> do
    port <- getAttrValue "port" -< onEvent
    eventActions <- listA parseEventAction -< onEvent
    returnA -< OnEvent port eventActions

parseEventHandler = parseOnStart `orElse` (parseOnCondition `orElse` parseOnEvent)

parseDynamics = atTag "Dynamics" >>>
  proc dynamics -> do
    stateVariables  <- listA parseStateVariable  -< dynamics
    timeDerivatives <- listA parseTimeDerivative -< dynamics
    eventHandlers   <- listA parseEventHandler   -< dynamics
    returnA -< Just $ Dynamics stateVariables timeDerivatives eventHandlers
    
parseComponentType = atTag "ComponentType" >>>
  proc compType -> do
    name            <- getAttrValue "name"               -< compType
    extends         <- getAttrValue "extends"            -< compType
    parameters      <- listA parseParameter              -< compType
    fixedParameters <- listA parseFixed                  -< compType
    exposures       <- listA parseExposure               -< compType
    eventPorts      <- listA parseEventPort              -< compType
    dynamics        <- withDefault parseDynamics Nothing -< compType
    returnA -< ComponentType name extends parameters fixedParameters exposures eventPorts dynamics
       
parseLems = atTag "Lems" >>>
  proc lems -> do
    includes   <- listA parseInclude        -< lems
    dimensions <- listA parseDimension      -< lems
    units      <- listA parseUnit           -< lems
    assertions <- listA parseAssertion      -< lems
    constants  <- listA parseConstant       -< lems
    compTypes  <- listA parseComponentType  -< lems
    returnA -< Lems includes dimensions units assertions constants compTypes [] Nothing


parseXML xmlText = readString [ withValidate no
                              , withRemoveWS yes  -- throw away formating WS
                              ] xmlText



test1 = do
  contents <- readFile "/home/gautham/work/NeuroML/LEMS/examples/example1.xml"
  models <- runX (parseXML contents >>> parseLems)
  putStrLn $ show $ (head models)
  putStrLn ""
  putStrLn $ show $ (lemsCompTypes (head models) !! 2)
  
