{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wall #-}

module Example
  ( -- * State data
    X (..)
  , Y (..)
  , Z (..)

    -- * State transitions
  , Xyz (..)

    -- * Transition helper functions
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
  transition :: Xyz i o -> i -> o
  transition = \case
    InitX -> \() -> X
    InitY n -> \() -> Y n
    XToY n -> \X -> Y n
    YToZ -> \(Y _) -> Z

initX :: X
initX = init InitX

initY :: Int -> Y
initY n = init (InitY n)

xToY :: Int -> X -> Y
xToY n = transition (XToY n)

yToZ :: Y -> Z
yToZ = transition YToZ

_example :: Z
_example =
  let
    x = initX
    y = xToY 42 x
    z = yToZ y
  in
    z
