module Workflow
  ( Workflow
  , initialize
  , transition
  , module Workflow.Concrete
  , module Workflow.Abstract
  , module Workflow.Abstract.TH
  , module Workflow.Info
  )
where

import Workflow.Abstract
import Workflow.Abstract.TH
import Workflow.Concrete
import Workflow.Info

type Workflow w = (AbstractWorkflow w, ConcreteWorkflow w)

initialize :: (ConcreteWorkflow w, Functor f) => w f () o -> f (State w o)
initialize = initializeC

transition :: (ConcreteWorkflow w, Functor f) => w f i o -> State w i -> f (State w o)
transition = transitionC
