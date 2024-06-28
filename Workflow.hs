module Workflow
  ( module AbstractWorkflow
  , module ConcreteWorkflow
  , Workflow
  , init
  , trans
  )
where

import AbstractWorkflow
import ConcreteWorkflow
import Prelude hiding (init)

type Workflow w = (AbstractWorkflow w, ConcreteWorkflow w)

init :: (ConcreteWorkflow w, Functor f) => w f () o -> f (State w o)
init = initC

trans :: (ConcreteWorkflow w, Functor f) => w f i o -> State w i -> f (State w o)
trans = transC
