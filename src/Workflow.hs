module Workflow
  ( module AbstractWorkflow
  , module ConcreteWorkflow
  , module Info
  , module Graph
  , module TH
  , Workflow
  , init
  , trans
  , stateTag
  )
where

import AbstractWorkflow
import ConcreteWorkflow
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Graph
import Info
import Prelude hiding (init)
import TH
import Type.Reflection (Typeable)

type Workflow w = (AbstractWorkflow w, ConcreteWorkflow w)

init :: (ConcreteWorkflow w, Functor f) => w f () o -> f (State w o)
init = initC

trans :: (ConcreteWorkflow w, Functor f) => w f i o -> State w i -> f (State w o)
trans = transC

stateTag :: forall s w. (Workflow w, Typeable s) => State w s -> StateTag w s
stateTag _ = fromMaybe (error "unreachable") $ stateTag' (Proxy @s)
