{-# OPTIONS_GHC -Wall #-}

module Lib
  ( StateMachine (..)
  )
where

class StateMachine t where
  init :: t () o -> o
  init t = transition t ()

  transition :: t i o -> i -> o
