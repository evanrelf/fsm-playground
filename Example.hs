{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wall #-}

module Example where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Functor.Identity (Identity (..))
import Lib
import Prelude hiding (init)

data X = X

data Y = Y Int

data Z = Z

data Xyz f i o where
  InitX :: Xyz Identity () X
  InitY :: Int -> Xyz Identity () Y
  XToY :: Int -> Xyz Identity X Y
  YToZ :: Xyz IO Y Z

instance StateMachine Xyz where
  transitionRaw :: Xyz f i o -> i -> f o
  transitionRaw = \case
    InitX -> \() -> pure X
    InitY n -> \() -> pure (Y n)
    XToY n -> \X -> pure (Y n)
    YToZ -> \(Y _) -> putStrLn "Z!" *> pure Z

-- Helper functions, totally optional

-- `X` is just data, `State s X` is a _state_ with that data. The latter is
-- correct by construction. In other words, if data is wrapped with `State`, you
-- know it's in a valid state and can only change via valid transitions.

initX :: State Xyz X
initX = runIdentity $ init InitX

initY :: Int -> State Xyz Y
initY n = runIdentity $ init (InitY n)

xToY :: Int -> State Xyz X -> State Xyz Y
xToY n i = runIdentity $ transition (XToY n) i

yToZ :: MonadIO m => State Xyz Y -> m (State Xyz Z)
yToZ i = liftIO $ transition YToZ i

_exampleXyz :: IO (State Xyz Z)
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

data EvilXyz f i o where
  EvilInitZ :: EvilXyz Identity () Z
  EvilZToX :: EvilXyz Identity Z X

instance StateMachine EvilXyz where
  transitionRaw :: EvilXyz f i o -> i -> f o
  transitionRaw = \case
    EvilInitZ -> \() -> pure Z
    EvilZToX -> \Z -> pure X

-- Doesn't compile if you lie about the state machine type, preventing invalid
-- `init`s or `transition`s (outside the canonical ones defined by the type
-- class).

-- _evilInitZ :: State Xyz Z
_evilInitZ :: State EvilXyz Z
_evilInitZ = runIdentity $ init EvilInitZ

-- _evilZToX :: State Xyz Z -> State Xyz X
_evilZToX :: State EvilXyz Z -> State EvilXyz X
_evilZToX i = runIdentity $ transition EvilZToX i
