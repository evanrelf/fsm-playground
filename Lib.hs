{-# OPTIONS_GHC -Wall #-}

module Lib
  ( State
  , StateMachine (..)
  , init
  , transition
  )
where

import Prelude hiding (init)

-- A completely opaque, correct by construction container for state. Can only
-- be constructed by calling `init` or `transition`.
--
-- In other words, `MyState` is just a regular value that you can inspect and
-- modify arbitrarily, but `State MyFsm MyState` is a `MyState` that is
-- guaranteed to have been produced by lawful transitions in the `MyFsm` state
-- machine.
newtype State s a = UnsafeState a

class StateMachine t where
  -- If you describe how to modify your underlying state data for all
  -- transitions in this state machine:
  transitionRaw :: Functor f => t f i o -> i -> f o

-- ...then you get type-safe state machine transitions:

init :: (StateMachine t, Functor f) => t f () o -> f (State t o)
init t = UnsafeState <$> transitionRaw t ()

transition :: (StateMachine t, Functor f) => t f i o -> State t i -> f (State t o)
transition t (UnsafeState i) = UnsafeState <$> transitionRaw t i
