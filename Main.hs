module Main (main) where

import Example
import Workflow

main :: IO ()
main = do
  putStrLn $ dot (workflowInfo @Xyz)
