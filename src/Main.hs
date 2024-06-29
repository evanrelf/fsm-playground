module Main (main) where

import Examples
import Workflow

main :: IO ()
main = do
  putStrLn $ dot (workflowInfo @Xyz)
