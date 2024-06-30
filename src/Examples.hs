{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Examples
  ( module Examples
  , module Workflow.Example.Xyz
  )
where

import Data.Function ((&))
import Data.Functor.Identity (Identity (..))
import Data.Monoid (Sum (..))
import Data.Proxy (Proxy (..))
import Effectful (Eff, (:>), runPureEff)
import Effectful.Writer.Dynamic (Writer, runWriterLocal, tell)
import GHC.TypeLits (KnownNat, type (-))
import Prelude hiding (init)
import Workflow
import Workflow.Example.Xyz

-- Pretend this is in its own module, and the constructor is not exported.
newtype Box n a = UnsafeBox a

data TimeRelease f i o where
  Lock :: KnownNat n => Proxy n -> a -> TimeRelease Identity () (Box n a)
  Tick :: TimeRelease Identity (Box n a) (Box (n - 1) a)
  Unlock :: TimeRelease Identity (Box 0 a) a

instance ConcreteWorkflow TimeRelease where
  transImpl :: TimeRelease f i o -> i -> f o
  transImpl = \case
    Lock _ a -> \() -> pure (UnsafeBox a)
    Tick -> \(UnsafeBox a) -> pure (UnsafeBox a)
    Unlock -> \(UnsafeBox a) -> pure a

lock :: forall n a. KnownNat n => a -> State TimeRelease (Box n a)
lock a = runIdentity $ init (Lock (Proxy @n) a)

tick :: State TimeRelease (Box n a) -> State TimeRelease (Box (n - 1) a)
tick i = runIdentity $ trans Tick i

unlock :: State TimeRelease (Box 0 a) -> a
unlock i = getState $ runIdentity $ trans Unlock i

_exampleTimeRelease :: Bool
_exampleTimeRelease =
  (42 :: Int)
    & lock @3
    -- Comment one of these `tick`s out and watch the program fail to compile!
    -- GHC enforces that you call `tick` the correct number of times to
    -- decrement the counter to zero.
    & tick
    & tick
    & tick
    & unlock
    & (== 42)

--------------------------------------------------------------------------------

-- TODO: Embed a `TimeRelease` into each `TrafficLight` state, to mimic the time
-- you wait at a traffic light, and demonstrate combining workflows.

data Red = Red

data Yellow = Yellow

data Green = Green

data TrafficLight f i o where
  InitRed :: TrafficLight (Eff es) () Red
  -- Count the number of times you go from red to green
  Go :: Writer (Sum Int) :> es => TrafficLight (Eff es) Red Green
  Slow :: TrafficLight (Eff es) Green Yellow
  Stop :: TrafficLight (Eff es) Yellow Red

instance ConcreteWorkflow TrafficLight where
  transImpl :: TrafficLight f i o -> i -> f o
  transImpl = \case
    InitRed -> \() -> pure Red
    -- Type annotation only required here because I'm not using
    -- `effectful-plugin` for better type inference.
    Go -> \Red -> tell (Sum @Int 1) *> pure Green
    Slow -> \Green -> pure Yellow
    Stop -> \Yellow -> pure Red

initRed :: Eff es (State TrafficLight Red)
initRed = init InitRed

go :: Writer (Sum Int) :> es => State TrafficLight Red -> Eff es (State TrafficLight Green)
go i = trans Go i

slow :: State TrafficLight Green -> Eff es (State TrafficLight Yellow)
slow i = trans Slow i

stop :: State TrafficLight Yellow -> Eff es (State TrafficLight Red)
stop i = trans Stop i

_exampleTrafficLight :: (String, Sum Int)
_exampleTrafficLight = runPureEff . runWriterLocal $ do
  red1 <- initRed
  green <- go red1
  yellow <- slow green
  red2 <- stop yellow
  case (getState red1, getState red2) of
    (Red, Red) -> pure "they're equal!"
