module Workflow
  ( module AbstractWorkflow
  , module ConcreteWorkflow
  , Workflow
  )
where

import AbstractWorkflow
import ConcreteWorkflow

type Workflow w = (AbstractWorkflow w, ConcreteWorkflow w)
