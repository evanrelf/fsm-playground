module Workflow
  ( module Workflow.Abstract
  , module Workflow.Abstract.TH
  , module Workflow.Concrete
  , module Workflow.Info
  , module Workflow.Graph
  , Workflow
  , init
  , trans
  , stateTag
  )
where

import Data.Maybe (fromMaybe)
import Prelude hiding (init)
import Type.Reflection (Typeable)
import Workflow.Abstract
import Workflow.Abstract.TH
import Workflow.Concrete
import Workflow.Graph
import Workflow.Info

type Workflow w = (AbstractWorkflow w, ConcreteWorkflow w)

init :: (ConcreteWorkflow w, Functor f) => w f () o -> f (State w o)
init = initC

trans :: (ConcreteWorkflow w, Functor f) => w f i o -> State w i -> f (State w o)
trans = transC

stateTag :: forall s w. (Workflow w, Typeable s) => State w s -> StateTag w s
stateTag _ = fromMaybe (error "unreachable") $ stateTag' @w @s
