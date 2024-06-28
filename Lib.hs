{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}

module Lib
  ( -- * Abstract
    StateInfo (..)
  , TransitionKind (..)
  , TransitionInfo (..)
  , WorkflowInfo (..)
  , SomeStateTag (..)
  , SomeTransitionTag (..)
  , AbstractWorkflow (..)
  , stateInfos
  , transitionInfos

    -- * Concrete
  , State
  , Workflow (..)
  , init
  , transition
  )
where

import Data.Kind (Type)
import Prelude hiding (init)

-- TODO: Derive `AbstractWorkflow` instances with Template Haskell

data StateInfo w = StateInfo
  { stateInfoName :: String
  , stateInfoDescription :: String
  }

data SomeStateTag w = forall s. SomeStateTag (StateTag w s)

data TransitionKind
  = TransitionKind_Init
  | TransitionKind_Transition
  | TransitionKind_Exit

data TransitionInfo w = TransitionInfo
  { transitionInfoName :: String
  , transitionInfoDescription :: String
  , transitionInfoKind :: TransitionKind
  }

data SomeTransitionTag w = forall i o. SomeTransitionTag (TransitionTag w i o)

data WorkflowInfo w = WorkflowInfo
  { workflowInfoName :: String
  , workflowInfoDescription :: String
  }

-- TODO: Rename this to something less enterprise Java-y
class AbstractWorkflow w where
  data StateTag w :: Type -> Type
  data TransitionTag w :: Type -> Type -> Type
  states :: [SomeStateTag w]
  transitions :: [SomeTransitionTag w]
  workflowInfo :: WorkflowInfo w
  stateInfo :: StateTag w s -> StateInfo w
  transitionInfo :: TransitionTag w i o -> TransitionInfo w

stateInfos :: AbstractWorkflow w => [StateInfo w]
stateInfos = map (\(SomeStateTag tag) -> stateInfo tag) states

transitionInfos :: AbstractWorkflow w => [TransitionInfo w]
transitionInfos =
  map (\(SomeTransitionTag tag) -> transitionInfo tag) transitions

-- A completely opaque, correct by construction container for state. Can only
-- be constructed by calling `init` or `transition`.
--
-- In other words, `MyState` is just a regular value that you can inspect and
-- modify arbitrarily, but `State MyWorkflow MyState` is a `MyState` that is
-- guaranteed to have been produced by lawful transitions in the `MyWorkflow`
-- workflow.
newtype State s a = UnsafeState a

class Workflow w where
  -- If you describe how to modify your underlying state data for all
  -- transitions in this workflow:
  transitionRaw :: Functor f => w f i o -> i -> f o

-- ...then you get type-safe workflow transitions:

init :: (Workflow w, Functor f) => w f () o -> f (State w o)
init w = UnsafeState <$> transitionRaw w ()

transition :: (Workflow w, Functor f) => w f i o -> State w i -> f (State w o)
transition w (UnsafeState i) = UnsafeState <$> transitionRaw w i
