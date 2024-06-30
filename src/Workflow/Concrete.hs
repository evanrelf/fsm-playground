{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Workflow.Concrete
  ( ConcreteWorkflow (..)
  , initC
  , transC
  , State (State)
  , getState
  )
where

import Data.Hashable (Hashable)
import GHC.Records (HasField (..))

-- | If you describe how to modify your underlying state data for all
-- transitions in this workflow, then you get type-safe workflow transitions
-- with `State` + `initC` + `transC`.
class ConcreteWorkflow w where
  transImpl :: Functor f => w f i o -> i -> f o

initC :: (ConcreteWorkflow w, Functor f) => w f () o -> f (State w o)
initC w = UnsafeState <$> transImpl w ()

transC :: (ConcreteWorkflow w, Functor f) => w f i o -> State w i -> f (State w o)
transC w (UnsafeState i) = UnsafeState <$> transImpl w i

-- | A completely opaque, correct by construction container for state. Can only
-- be constructed by calling `initC` or `transC`.
--
-- In other words, `MyState` is just a regular value that you can inspect and
-- modify arbitrarily, but `State MyWorkflow MyState` is a `MyState` that is
-- guaranteed to have been produced by lawful transitions in the `MyWorkflow`
-- workflow.
newtype State w a = UnsafeState a
  deriving newtype (Eq, Ord, Hashable)
  deriving stock (Show)

instance HasField x r a => HasField x (State w r) a where
  getField (UnsafeState r) = getField @x r

-- | Read-only pattern for unwrapping `State`s. Cannot be used to construct new
-- `State`s.
--
-- Pattern matching with this is equivalent to calling `getState`.
pattern State :: a -> State w a
pattern State a <- UnsafeState a
-- TODO: Make this explicitly bidirectional, and add a type error on the side
-- that tries to construct a `State`, to explain why that's not allowed?
-- Otherwise it's a vague error from GHC about the pattern being unidirectional.

{-# COMPLETE State #-}

getState :: State w a -> a
getState (UnsafeState a) = a
