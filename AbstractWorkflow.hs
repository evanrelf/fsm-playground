{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}

module AbstractWorkflow
  ( StateInfo (..)
  , TransitionKind (..)
  , TransitionInfo (..)
  , WorkflowInfo (..)
  , SomeStateTag (..)
  , SomeTransitionTag (..)
  , AbstractWorkflow (..)
  , stateInfos
  , transitionInfos
  , AbstractState
  , initA
  , transitionA
  )
where

import Data.Kind (Type)

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

data AbstractState s = AbstractState

initA :: AbstractWorkflow w => TransitionTag w () o -> AbstractState o
initA _ = AbstractState

transitionA :: AbstractWorkflow w => TransitionTag w i o -> AbstractState i -> AbstractState o
transitionA _ _ = AbstractState