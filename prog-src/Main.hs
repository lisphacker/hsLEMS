module Main where

import Protolude

import Data.Semigroup ((<>))
import Options.Applicative

import Data.String

import Language.NeuroML.LEMS.Parser (parseLemsXMLFile)

data CommandLineOptions = CommandLineOptions
  { includeDirectories :: ![String]
  , xmlFile            :: !String    
  } deriving (Show)

version :: String
version = "0.1.0.0"

commandLineParser :: ParserInfo CommandLineOptions
commandLineParser = info
                    (helper <*> versionOption <*> programOptions)
                    (fullDesc <> progDesc "NeuroML2/LEMS compiler" <>
                     header
                     "hslems - A NeuroML2/LEMS compiler written in Haskell")
  where
    versionOption :: Parser (a -> a)
    versionOption = infoOption version (long "version" <> help "Show version")
    
    programOptions :: Parser CommandLineOptions
    programOptions = CommandLineOptions <$> includeDirectoryOptions <*> xmlFileArg

    xmlFileArg :: Parser String
    xmlFileArg = argument str
                 (metavar "<XML file>" <> help "Input NeuroML2/LEMS model to be compiled")

    includeDirectoryOptions :: Parser [String]
    includeDirectoryOptions = many (strOption (short 'I'
                                               <> long "include"
                                               <> metavar "<Include directory>"
                                               <> help "Directory to search for included files"))
    
  
main :: IO ()
main = do
  opts <- execParser commandLineParser
  lemsModel <- parseLemsXMLFile (includeDirectories opts) (xmlFile opts)
  putStrLn $ (show lemsModel :: Text)
  --putStrLn $ (show opts :: Text)



