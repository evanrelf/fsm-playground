{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Workflow.Example.TimeRelease
  ( Box
  , TimeRelease (..)
  , lock
  , tick
  , unlock
  )
where

import Data.Function ((&))
import Data.Functor.Identity (Identity (..))
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, type (-))
import Prelude hiding (init)
import Workflow

-- | An opaque box that can only be manipulated via the `TimeRelease` workflow.
newtype Box n a = UnsafeBox a

-- | Cannot implement `AbstractWorkflow` for this workflow as written, because
-- the number of state types is unbounded (you can set `n` to any natural
-- number).
data TimeRelease f i o where
  -- | Lock a value `a` away for `n` ticks.
  Lock :: KnownNat n => Proxy n -> a -> TimeRelease Identity () (Box n a)
  -- | Tick a box's timer down by 1.
  Tick :: TimeRelease Identity (Box n a) (Box (n - 1) a)
  -- | Unlock a box whose timer has finished and return the value inside.
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
