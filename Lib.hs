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

getState :: State s a -> a
getState (UnsafeState a) = a

class StateMachine t where
  transitionRaw :: Functor f => t f i o -> i -> f o

init :: (StateMachine t, Functor f) => t f () o -> f (State t o)
init t = UnsafeState <$> transitionRaw t ()

transition :: (StateMachine t, Functor f) => t f i o -> State t i -> f (State t o)
transition t (UnsafeState i) = UnsafeState <$> transitionRaw t i
