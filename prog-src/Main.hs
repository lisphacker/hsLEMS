module Main where

import System.Environment
import System.Directory

import Language.NeuroML.LEMS.Parser

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile $ args !! 0
  putStrLn $ show $ parseXML contents



