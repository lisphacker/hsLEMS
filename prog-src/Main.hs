module Main where

import Protolude
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("hsLems" :: Text)
