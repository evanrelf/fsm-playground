{-# OPTIONS_GHC -Wall #-}

module ConcreteWorkflow
  ( State
  , ConcreteWorkflow (..)
  , initC
  , transitionC
  )
where

-- A completely opaque, correct by construction container for state. Can only
-- be constructed by calling `init` or `transition`.
--
-- In other words, `MyState` is just a regular value that you can inspect and
-- modify arbitrarily, but `State MyWorkflow MyState` is a `MyState` that is
-- guaranteed to have been produced by lawful transitions in the `MyWorkflow`
-- workflow.
newtype State s a = UnsafeState a

class ConcreteWorkflow w where
  -- If you describe how to modify your underlying state data for all
  -- transitions in this workflow:
  transitionRaw :: Functor f => w f i o -> i -> f o

-- ...then you get type-safe workflow transitions:

initC :: (ConcreteWorkflow w, Functor f) => w f () o -> f (State w o)
initC w = UnsafeState <$> transitionRaw w ()

transitionC :: (ConcreteWorkflow w, Functor f) => w f i o -> State w i -> f (State w o)
transitionC w (UnsafeState i) = UnsafeState <$> transitionRaw w i
