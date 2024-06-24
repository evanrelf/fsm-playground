{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wall #-}

module Example
  ( X (..)
  , Y (..)
  , Z (..)
  , Xyz (..)
  , initX
  , initY
  , xToY
  , yToZ
  )
where

import Lib
import Prelude hiding (init)

data X = X

data Y = Y Int

data Z = Z

data Xyz i o where
  InitX :: Xyz () X
  InitY :: Int -> Xyz () Y
  XToY :: Int -> Xyz X Y
  YToZ :: Xyz Y Z

instance StateMachine Xyz where
  transitionRaw :: Xyz i o -> i -> o
  transitionRaw = \case
    InitX -> \() -> X
    InitY n -> \() -> Y n
    XToY n -> \X -> Y n
    YToZ -> \(Y _) -> Z

-- Helper functions, totally optional

-- `X` is just data, `State s X` is a _state_ with that data. The latter is
-- correct by construction. In other words, if data is wrapped with `State`, you
-- know it's in a valid state and can only change via valid transitions.

initX :: State Xyz X
initX = init InitX

initY :: Int -> State Xyz Y
initY n = init (InitY n)

xToY :: Int -> State Xyz X -> State Xyz Y
xToY n = transition (XToY n)

yToZ :: State Xyz Y -> State Xyz Z
yToZ = transition YToZ

_exampleXyz :: State Xyz Z
_exampleXyz =
  let
    x = initX
    y = xToY 42 x
    z = yToZ y
  in
    z

-- Attempting to obtain an `State s Z` with `init` rather than `transition` by
-- creating a new state machine with the same data where that's a legal
-- operation.

data EvilXyz i o where
  EvilInitZ :: EvilXyz () Z
  EvilZToX :: EvilXyz Z X

instance StateMachine EvilXyz where
  transitionRaw :: EvilXyz i o -> i -> o
  transitionRaw = \case
    EvilInitZ -> \() -> Z
    EvilZToX -> \Z -> X

-- Doesn't compile if you lie about the state machine type, preventing invalid
-- `init`s or `transition`s (outside the canonical ones defined by the type
-- class).

-- _evilInitZ :: State Xyz Z
_evilInitZ :: State EvilXyz Z
_evilInitZ = init EvilInitZ

-- _evilZToX :: State Xyz Z -> State Xyz X
_evilZToX :: State EvilXyz Z -> State EvilXyz X
_evilZToX = transition EvilZToX
