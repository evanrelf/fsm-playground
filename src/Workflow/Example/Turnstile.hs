{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Workflow.Example.Turnstile where

import Data.Functor.Identity (Identity)
import Workflow

data Locked = Locked

data Unlocked = Unlocked

data Turnstile f i o where
  Init :: Turnstile Identity () Locked
  Push :: Turnstile Identity Unlocked Locked
  Coin :: Turnstile Identity Locked Unlocked

instance Workflow Turnstile where
  transitionRaw = \case
    Init -> \() -> pure Locked
    Push -> \Unlocked -> pure Locked
    Coin -> \Locked -> pure Unlocked

_exampleTurnstile :: Identity (State Turnstile Locked)
_exampleTurnstile =
      initialize Init
  >>= transition Coin
  >>= transition Push
