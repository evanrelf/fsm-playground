{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wall #-}

module Example where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Proxy (Proxy (..))
import Data.Void (Void)
import GHC.TypeLits (KnownNat, type (-))
import Lib
import Prelude hiding (init)

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

-- Pretend this is in its own module, and the constructor is not exported.
newtype Box n a = UnsafeBox a

data TimeRelease f i o where
  Lock :: KnownNat n => Proxy n -> a -> TimeRelease Identity () (Box n a)
  Tick :: TimeRelease Identity (Box n a) (Box (n - 1) a)
  Unlock :: TimeRelease (Const a) (Box 0 a) Void

instance StateMachine TimeRelease where
  transitionRaw :: TimeRelease f i o -> i -> f o
  transitionRaw = \case
    Lock _ a -> \() -> pure (UnsafeBox a)
    Tick -> \(UnsafeBox a) -> pure (UnsafeBox a)
    Unlock -> \(UnsafeBox a) -> Const a

lock :: forall n a. KnownNat n => a -> State TimeRelease (Box n a)
lock a = runIdentity $ init (Lock (Proxy @n) a)

tick :: State TimeRelease (Box n a) -> State TimeRelease (Box (n - 1) a)
tick i = runIdentity $ transition Tick i

unlock :: State TimeRelease (Box 0 a) -> a
unlock i = getConst $ transition Unlock i

_exampleTimeRelease :: Bool
_exampleTimeRelease =
  let
    x = 42 :: Int
    b3 = lock @3 x
    b2 = tick b3
    b1 = tick b2
    b0 = tick b1
    y = unlock b0
  in
    x == y

--------------------------------------------------------------------------------

-- TODO: Embed a `TimeRelease` into each `TrafficLight` state, to mimic the time
-- you wait at a traffic light, and demonstrate combining state machines.

data Red = Red

data Yellow = Yellow

data Green = Green

data TrafficLight f i o where
  InitRed :: TrafficLight Identity () Red
  Go :: TrafficLight Identity Red Green
  Slow :: TrafficLight Identity Green Yellow
  Stop :: TrafficLight Identity Yellow Red

instance StateMachine TrafficLight where
  transitionRaw :: TrafficLight f i o -> i -> f o
  transitionRaw = \case
    InitRed -> \() -> pure Red
    Go -> \Red -> pure Green
    Slow -> \Green -> pure Yellow
    Stop -> \Yellow -> pure Red

initRed :: State TrafficLight Red
initRed = runIdentity $ init InitRed

go :: State TrafficLight Red -> State TrafficLight Green
go i = runIdentity $ transition Go i

slow :: State TrafficLight Green -> State TrafficLight Yellow
slow i = runIdentity $ transition Slow i

stop :: State TrafficLight Yellow -> State TrafficLight Red
stop i = runIdentity $ transition Stop i

_exampleTrafficLight :: State TrafficLight Red
_exampleTrafficLight =
  let
    red1 = initRed
    green = go red1
    yellow = slow green
    red2 = stop yellow
  in
    red2
