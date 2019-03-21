module Main where

import Protolude

import Data.Semigroup ((<>))
import Options.Applicative

import Data.String

import qualified Language.NeuroML.LEMS.Parser as P
import Language.NeuroML.LEMS.Semantics.Analyzer (processParseTree)


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
  eiParseTree <- P.parseLemsXMLFile (includeDirectories opts) (xmlFile opts)
  let eiModel = eiParseTree >>= processParseTree
  --putStrLn $ ((show $ P.lemsCompTypes <$> eiParseTree) :: Text)
  putStrLn $ (show eiModel :: Text)



