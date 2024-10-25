{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

module Workflow.Concrete
  ( ConcreteWorkflow (..)
  , initialize
  , transition
  , State (State)
  , getState
  , UnsafeStateFromJSON (..)
  )
where

import Data.Aeson
import Data.Hashable (Hashable)
import Data.Kind (Constraint)
import GHC.Records (HasField (..))
import GHC.TypeError (ErrorMessage (..), TypeError)

-- | If you describe how to modify your underlying state data for all
-- transitions in this workflow, then you get type-safe workflow transitions
-- with `State` + `initC` + `transC`.
class ConcreteWorkflow w where
  transitionRaw :: w f i o -> i -> f o

initialize :: (ConcreteWorkflow w, Functor f) => w f () o -> f (State w o)
initialize w = UnsafeState <$> transitionRaw w ()

transition :: (ConcreteWorkflow w, Functor f) => w f i o -> State w i -> f (State w o)
transition w (State i) = UnsafeState <$> transitionRaw w i

-- | A completely opaque, correct by construction container for state. Can only
-- be constructed by calling `initC` or `transC`.
--
-- In other words, `MyState` is just a regular value that you can inspect and
-- modify arbitrarily, but @'State' MyWorkflow MyState@ is a `MyState` that is
-- guaranteed to have been produced by lawful transitions in the `MyWorkflow`
-- workflow.
newtype State w a = UnsafeState a
  deriving newtype (Eq, Ord, Hashable, ToJSON)
  deriving stock (Show)

-- | Use all of `r`'s fields with @'State' w r@, no unwrapping necessary.
instance HasField x r a => HasField x (State w r) a where
  getField = getField @x . getState

-- | Read-only pattern for unwrapping `State`s. Cannot be used to construct new
-- `State`s.
--
-- Pattern matching with this is equivalent to calling `getState`.
pattern State :: a -> State w a
pattern State a <- UnsafeState a
{-# COMPLETE State #-}
-- TODO: Make this explicitly bidirectional, and add a type error on the side
-- that tries to construct a `State`, to explain why that's not allowed?
-- Otherwise it's a vague error from GHC about the pattern being unidirectional.

getState :: State w a -> a
getState (State a) = a

data UnsafeStateFromJSON w a = UnsafeStateFromJSON (State w a)

type UnsafetyError :: k1 -> k2 -> Constraint
type UnsafetyError w a = TypeError
  ( Text "Decoding a `State " :<>: ShowType w :<>: Text " " :<>: ShowType a :<>:
    Text "` from JSON is unsafe! You could construct an illegal state by " :<>:
    Text "decoding arbitrary JSON.\n" :$$: Text "If you're confident the " :<>:
    Text "JSON represents a legal state for this workflow, you can bypass " :<>:
    Text "this error using the `UnsafeStateFromJSON` wrapper type."
  )

-- | Decoding from JSON is unsafe! You could construct an illegal state by
-- decoding arbitrary JSON.
--
-- If you're confident the JSON represents a legal state for this workflow, use
-- the `UnsafeStateFromJSON` wrapper type.
instance UnsafetyError w a => FromJSON (State w a) where
  parseJSON = error "unreachable"

instance (ConcreteWorkflow w, FromJSON a) => FromJSON (UnsafeStateFromJSON w a) where
  parseJSON = fmap (UnsafeStateFromJSON . UnsafeState) . parseJSON
