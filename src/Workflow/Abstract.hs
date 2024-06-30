{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Workflow.Abstract
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
import Type.Reflection (Typeable)
import Workflow.Info

-- TODO: Derive instances with Template Haskell
-- TODO: States and transitions need to be enumerable and bounded (not
-- necessarily `Enum` and `Bounded`, though that'd be nice)
class AbstractWorkflow w where
  data StateTag w :: Type -> Type
  data TransitionTag w :: Type -> Type -> Type
  states :: [SomeStateTag w]
  transitions :: [SomeTransitionTag w]
  stateTag' :: Typeable a => Maybe (StateTag w a)
  transitionTag :: w f i o -> TransitionTag w i o
  workflowInfo :: WorkflowInfo
  stateInfo :: StateTag w s -> StateInfo
  transitionInfo :: TransitionTag w i o -> TransitionInfo

stateInfos :: forall w. AbstractWorkflow w => [StateInfo]
stateInfos = map (\(SomeStateTag tag) -> stateInfo tag) (states @w)

transitionInfos :: forall w. AbstractWorkflow w => [TransitionInfo]
transitionInfos =
  map (\(SomeTransitionTag tag) -> transitionInfo tag) (transitions @w)

data SomeStateTag w
   = forall s. (AbstractWorkflow w, Typeable s)
  => SomeStateTag (StateTag w s)

deriving stock instance
  (forall s. Show (StateTag w s))
  => Show (SomeStateTag w)

data SomeTransitionTag w
   = forall i o. (AbstractWorkflow w, Typeable i, Typeable o)
  => SomeTransitionTag (TransitionTag w i o)

deriving stock instance
  (forall i o. Show (TransitionTag w i o))
  => Show (SomeTransitionTag w)

data AbstractState w a = AbstractState
  deriving stock (Show)

initA :: AbstractWorkflow w => TransitionTag w () o -> AbstractState w o
initA _ = AbstractState

transA :: AbstractWorkflow w => TransitionTag w i o -> AbstractState w i -> AbstractState w o
transA _ AbstractState = AbstractState
