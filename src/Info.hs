{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Info
  ( WorkflowInfo (..)
  , StateInfo (..)
  , TransitionInfo (..)
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data WorkflowInfo = WorkflowInfo
  { name :: String
  , description :: String
  , states :: [StateInfo]
  , transitions :: [TransitionInfo]
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

data StateInfo = StateInfo
  { name :: String
  , description :: String
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

data TransitionInfo = TransitionInfo
  { name :: String
  , description :: String
  , input :: Maybe StateInfo
  , output :: StateInfo
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)
