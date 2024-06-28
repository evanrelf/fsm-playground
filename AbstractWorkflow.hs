{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}

module AbstractWorkflow
  ( AbstractWorkflow (..)
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
import Info
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

stateInfos :: forall w. AbstractWorkflow w => [StateInfo]
stateInfos = map (\(SomeStateTag tag) -> stateInfo tag) (states @w)

transitionInfos :: forall w. AbstractWorkflow w => [TransitionInfo]
transitionInfos =
  map (\(SomeTransitionTag tag) -> transitionInfo tag) (transitions @w)

data SomeStateTag w
  = forall s. AbstractWorkflow w => SomeStateTag (StateTag w s)

data SomeTransitionTag w
  = forall i o. AbstractWorkflow w => SomeTransitionTag (TransitionTag w i o)

data AbstractState s = AbstractState

initA :: AbstractWorkflow w => TransitionTag w () o -> AbstractState o
initA _ = AbstractState

transA :: AbstractWorkflow w => TransitionTag w i o -> AbstractState i -> AbstractState o
transA _ AbstractState = AbstractState
