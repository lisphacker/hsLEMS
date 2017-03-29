module NeuroML.LEMS.Model.ComponentType where

import NeuroML.LEMS.Model.TypeClasses

-- | Definition of a component type
data ComponentType = ComponentType { compTypeName    :: String  -- ^ Name of the component type
                                   , compTypeExtends :: String  -- ^ Name of the base coomponent type
                                   } deriving (Show)

instance Named ComponentType where
  name = compTypeName

instance Extended ComponentType where
  extends = compTypeExtends

