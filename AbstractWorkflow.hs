{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}

module AbstractWorkflow
  ( AbstractWorkflow (..)

  , WorkflowInfo (..)
  , StateInfo (..)
  , TransitionInfo (..)
  , stateInfos
  , transitionInfos

  , SomeStateTag (..)
  , SomeTransitionTag (..)

  , AbstractState
  , initA
  , transA
  )
where

import Data.Kind (Type)
import Type.Reflection (Typeable)

-- TODO: Derive instances with Template Haskell
class AbstractWorkflow w where
  data StateTag w :: Type -> Type
  data TransitionTag w :: Type -> Type -> Type
  states :: [SomeStateTag w]
  transitions :: [SomeTransitionTag w]
  workflowInfo :: WorkflowInfo
  stateInfo :: StateTag w s -> StateInfo
  transitionInfo :: TransitionTag w i o -> TransitionInfo
  stateTag' :: (Typeable a, Typeable b) => proxy a -> Maybe (StateTag w b)
  transitionTag :: w f i o -> TransitionTag w i o

data WorkflowInfo = WorkflowInfo
  { name :: String
  , description :: String
  , states :: [StateInfo]
  , transitions :: [TransitionInfo]
  }

data StateInfo = StateInfo
  { name :: String
  , description :: String
  }

data TransitionInfo = TransitionInfo
  { name :: String
  , description :: String
  , input :: Maybe StateInfo
  , output :: StateInfo
  }

stateInfos :: forall w. AbstractWorkflow w => [StateInfo]
stateInfos = map (\(SomeStateTag tag) -> stateInfo @w tag) states

transitionInfos :: forall w. AbstractWorkflow w => [TransitionInfo]
transitionInfos =
  map (\(SomeTransitionTag tag) -> transitionInfo @w tag) transitions

data SomeStateTag w = forall s. SomeStateTag (StateTag w s)

data SomeTransitionTag w = forall i o. SomeTransitionTag (TransitionTag w i o)

data AbstractState s = AbstractState

initA :: AbstractWorkflow w => TransitionTag w () o -> AbstractState o
initA _ = AbstractState

transA :: AbstractWorkflow w => TransitionTag w i o -> AbstractState i -> AbstractState o
transA _ AbstractState = AbstractState
