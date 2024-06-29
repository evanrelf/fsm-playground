{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}

module ConcreteWorkflow
  ( State
  , getState
  , ConcreteWorkflow (..)
  , initC
  , transC
  )
where

import Data.Hashable (Hashable)

-- A completely opaque, correct by construction container for state. Can only
-- be constructed by calling `init` or `transition`.
--
-- In other words, `MyState` is just a regular value that you can inspect and
-- modify arbitrarily, but `State MyWorkflow MyState` is a `MyState` that is
-- guaranteed to have been produced by lawful transitions in the `MyWorkflow`
-- workflow.
newtype State s a = UnsafeState a
  deriving newtype (Eq, Ord, Hashable)
  deriving stock (Show)

getState :: State s a -> a
getState (UnsafeState a) = a

class ConcreteWorkflow w where
  -- If you describe how to modify your underlying state data for all
  -- transitions in this workflow:
  transImpl :: Functor f => w f i o -> i -> f o

-- ...then you get type-safe workflow transitions:

initC :: (ConcreteWorkflow w, Functor f) => w f () o -> f (State w o)
initC w = UnsafeState <$> transImpl w ()

transC :: (ConcreteWorkflow w, Functor f) => w f i o -> State w i -> f (State w o)
transC w (UnsafeState i) = UnsafeState <$> transImpl w i
