module Main where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile $ args !! 0
  putStrLn contents



