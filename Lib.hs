{-# OPTIONS_GHC -Wall #-}

module Lib
  ( State
  , getState
  , StateMachine (..)
  , init
  , transition
  )
where

import Prelude hiding (init)

newtype State s a = UnsafeState a

getState :: (forall s. State s a) -> a
getState (UnsafeState a) = a

class StateMachine t where
  transitionRaw :: t i o -> i -> o

init :: StateMachine t => t () o -> State t o
init t = UnsafeState (transitionRaw t ())

transition :: StateMachine t => t i o -> State t i -> State t o
transition t (UnsafeState i) = UnsafeState (transitionRaw t i)
