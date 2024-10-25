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
  , abstractInitialize
  , abstractTransition
  , AbstractState
  , SomeStateTag (..)
  , SomeTransitionTag (..)
  )
where

import Data.Hashable (Hashable (..), defaultHashWithSalt)
import Data.Kind (Type)
import Type.Reflection (Typeable)
import Workflow.Info

-- TODO: Derive instances with Template Haskell
class AbstractWorkflow w where
  data StateTag w :: Type -> Type
  data TransitionTag w :: Type -> Type -> Type
  states :: [SomeStateTag w]
  transitions :: [SomeTransitionTag w]
  stateTag :: Typeable a => Maybe (StateTag w a)
  transitionTag :: w f i o -> TransitionTag w i o
  workflowInfo :: WorkflowInfo
  stateInfo :: StateTag w s -> StateInfo
  transitionInfo :: TransitionTag w i o -> TransitionInfo

stateInfos :: forall w. AbstractWorkflow w => [StateInfo]
stateInfos = map (\(SomeStateTag tag) -> stateInfo tag) (states @w)

transitionInfos :: forall w. AbstractWorkflow w => [TransitionInfo]
transitionInfos =
  map (\(SomeTransitionTag tag) -> transitionInfo tag) (transitions @w)

abstractInitialize :: AbstractWorkflow w => TransitionTag w () o -> AbstractState w o
abstractInitialize _ = UnsafeAbstractState

abstractTransition :: AbstractWorkflow w => TransitionTag w i o -> AbstractState w i -> AbstractState w o
abstractTransition _ _ = UnsafeAbstractState

data AbstractState w a = UnsafeAbstractState
  deriving stock (Show, Eq, Ord)

instance Hashable (AbstractState w a) where
  hash _ = hash ()
  hashWithSalt = defaultHashWithSalt

data SomeStateTag w where
  SomeStateTag :: (AbstractWorkflow w, Typeable s) => StateTag w s -> SomeStateTag w

deriving stock instance
  (forall s. Show (StateTag w s))
  => Show (SomeStateTag w)

data SomeTransitionTag w where
  SomeTransitionTag :: (AbstractWorkflow w, Typeable i, Typeable o) => TransitionTag w i o -> SomeTransitionTag w

deriving stock instance
  (forall i o. Show (TransitionTag w i o))
  => Show (SomeTransitionTag w)
