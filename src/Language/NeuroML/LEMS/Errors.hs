{-|
Module      : Language.NeuroML.LEMS.Errors
Description : LEMS error codes
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gauthamg@gmail.com
Stability   : experimental
Portability : POSIX

Parser for a LEMS (XML) model.
-}
module Language.NeuroML.LEMS.Errors where

import Protolude
import Data.Text (Text)

-- | Error codes
data CompilerError = IncludeFileMissing Text -- ^ Include file missing
                   | InvalidLEMSXML Text     -- ^ Error parsing XML
                   | UnknownDimension Text   -- ^ Unknown dimension referenced from a unit.
                   | InvalidValue Text       -- ^ Invalid value
                   deriving (Show)
