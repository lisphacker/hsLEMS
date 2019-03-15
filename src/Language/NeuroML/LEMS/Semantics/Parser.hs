{-|
Module      : Language.NeuroML.LEMS.Semantics.Parser
Description : Expression and value parsers
Copyright   : (c) Gautham Ganapathy, 2019
License     : BSD
Maintainer  : gauthamg@gmail.com
Stability   : experimental
Portability : POSIX

LEMS model after being parsed from XML.
-}
module Language.NeuroML.LEMS.Semantics.Parser where

import Protolude hiding (dropWhile)

import Data.Text
import Data.Text.Read (double)
import Data.Char

import qualified Data.Map.Strict as M (lookup)

import Language.NeuroML.LEMS.Semantics.Model

parseValue :: UnitMap -> Text -> Maybe (NumericValue, Unit)
parseValue unitMap valueStr = let eiParsed = double valueStr
                              in case eiParsed of
                                   Left _ ->           Nothing
                                   Right (value, rest) -> let symbol = dropWhile isSpace $ dropWhileEnd isSpace rest
                                                          in case M.lookup symbol unitMap of
                                                               Just unit -> Just (value, unit)
                                                               Nothing   -> Nothing
                                                                  
