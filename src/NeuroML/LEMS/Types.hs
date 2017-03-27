module NeuroML.LEMS.Types where

data Dimension = Dimension { name               :: String
                           , mass               :: Int
                           , length             :: Int
                           , time               :: Int
                           , current            :: Int
                           , temperature        :: Int
                           , quantity           :: Int
                           , luminous_intensity :: Int
                           } deriving (Show)

data Unit = Unit { name      :: String
                 , symbol    :: String
                 , dimension :: String
                 , power10   :: Int
                 , scale     :: Double
                 , offset    :: Double
                 }
