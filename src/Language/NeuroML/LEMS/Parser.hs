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

parseComponentType = atTag "ComponentType" >>>
  proc compType -> do
    name      <- getAttrValue "name"    -< compType
    extends   <- getAttrValue "extends" -< compType
    returnA -< ComponentType name extends [] [] [] [] Nothing
       
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
