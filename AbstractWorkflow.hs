{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}

module AbstractWorkflow
  ( SomeStateTag (..)
  , SomeTransitionTag (..)
  , WorkflowInfo (..)
  , StateInfo (..)
  , TransitionInfo (..)
  , AbstractWorkflow (..)
  , stateInfos
  , transitionInfos
  , AbstractState
  , initA
  , transA
  )
where

import Data.Kind (Type)
import Type.Reflection (Typeable)

data SomeStateTag w where
  SomeStateTag :: StateTag w s -> SomeStateTag w

data SomeTransitionTag w where
  SomeTransitionTag :: TransitionTag w i o -> SomeTransitionTag w

data WorkflowInfo w = WorkflowInfo
  { name :: String
  , description :: String
  , states :: [StateInfo w]
  , transitions :: [TransitionInfo w]
  }

data StateInfo w = StateInfo
  { name :: String
  , description :: String
  }

data TransitionInfo w = TransitionInfo
  { name :: String
  , description :: String
  , input :: Maybe (StateInfo w)
  , output :: StateInfo w
  }

-- TODO: Derive instances with Template Haskell
class AbstractWorkflow w where
  data StateTag w :: Type -> Type
  data TransitionTag w :: Type -> Type -> Type
  states :: [SomeStateTag w]
  transitions :: [SomeTransitionTag w]
  workflowInfo :: WorkflowInfo w
  stateInfo :: StateTag w s -> StateInfo w
  transitionInfo :: TransitionTag w i o -> TransitionInfo w
  stateTag' :: (Typeable a, Typeable b) => proxy a -> Maybe (StateTag w b)
  transitionTag :: w f i o -> TransitionTag w i o

stateInfos :: AbstractWorkflow w => [StateInfo w]
stateInfos = map (\(SomeStateTag tag) -> stateInfo tag) states

transitionInfos :: AbstractWorkflow w => [TransitionInfo w]
transitionInfos =
  map (\(SomeTransitionTag tag) -> transitionInfo tag) transitions

data AbstractState s = AbstractState

initA :: AbstractWorkflow w => TransitionTag w () o -> AbstractState o
initA _ = AbstractState

transA :: AbstractWorkflow w => TransitionTag w i o -> AbstractState i -> AbstractState o
transA _ AbstractState = AbstractState
