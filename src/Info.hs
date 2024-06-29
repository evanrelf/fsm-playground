{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

{-# OPTIONS_GHC -Wall #-}

module Info
  ( WorkflowInfo (..)
  , StateInfo (..)
  , TransitionInfo (..)
  )
where

data WorkflowInfo = WorkflowInfo
  { name :: String
  , description :: String
  , states :: [StateInfo]
  , transitions :: [TransitionInfo]
  }

data StateInfo = StateInfo
  { name :: String
  , description :: String
  }

data TransitionInfo = TransitionInfo
  { name :: String
  , description :: String
  , input :: Maybe StateInfo
  , output :: StateInfo
  }
