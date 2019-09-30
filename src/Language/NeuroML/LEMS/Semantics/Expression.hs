{-|
Module      : Language.NeuroML.LEMS.Semantics.Expression
Description : Numeric values/expressions and their parsers
Copyright   : (c) Gautham Ganapathy, 2019
License     : BSD
Maintainer  : gauthamg@gmail.com
Stability   : experimental
Portability : POSIX

Representation of numeric expressions and values and their parsers
-}
module Language.NeuroML.LEMS.Semantics.Expression where

import Protolude

import Data.Attoparsec.Text

type NumericType = Double

data Expression v = Value v
                  | Term Text
                  | Add (Expression v) (Expression v)
                  | Sub (Expression v) (Expression v)
                  | Mul (Expression v) (Expression v)
                  | Div (Expression v) (Expression v)

parseNumericValue :: Parser (Expression NumericType)
parseNumericValue = undefined

parseExpression :: Text -> Expression NumericType
parseExpression text = case eitherResult $ parse parseNumericValue text of
  Left  err   -> undefined
  Right value -> undefined
