module Workflow
  ( Workflow
  , init
  , trans
  , module Workflow.Concrete
  , module Workflow.Abstract
  , module Workflow.Abstract.TH
  , module Workflow.Info
  )
where

import Prelude hiding (init)
import Workflow.Abstract
import Workflow.Abstract.TH
import Workflow.Concrete
import Workflow.Info

type Workflow w = (AbstractWorkflow w, ConcreteWorkflow w)

init :: (ConcreteWorkflow w, Functor f) => w f () o -> f (State w o)
init = initC

trans :: (ConcreteWorkflow w, Functor f) => w f i o -> State w i -> f (State w o)
trans = transC
