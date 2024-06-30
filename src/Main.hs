module Main (main) where

import Workflow
import Workflow.Example.Xyz

main :: IO ()
main = do
  putStrLn $ dot (workflowInfo @Xyz)
