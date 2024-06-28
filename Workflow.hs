module Workflow
  ( module AbstractWorkflow
  , module ConcreteWorkflow
  , Workflow
  , init
  , transition
  )
where

import AbstractWorkflow
import ConcreteWorkflow
import Prelude hiding (init)

type Workflow w = (AbstractWorkflow w, ConcreteWorkflow w)

init :: (ConcreteWorkflow w, Functor f) => w f () o -> f (State w o)
init = initC

transition :: (ConcreteWorkflow w, Functor f) => w f i o -> State w i -> f (State w o)
transition = transitionC
