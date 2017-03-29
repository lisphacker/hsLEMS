module NeuroML.LEMS.Model.TypeClasses where

-- | Type class for named objects
class Named a where
  name :: a -> String
  
-- | Type class for extended objects
class Extended a where
  extends :: a -> String
