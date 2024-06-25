{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module TH
  ( StateMachineTH (..)
  , derive
  )
where

import Data.Kind (Type)
import GHC.TypeError (ErrorMessage (..), TypeError)
import Language.Haskell.TH hiding (Type)

-- This Template Haskell stuff isn't used yet. Still a work in progress; nothing
-- to see here.

class StateMachineTH t where
  data StateTag t :: Type -> Type
  data TransitionTag t :: Type -> Type -> Type

-- TODO: Generate `StateMachineTH` instance
derive :: Name -> Q [Dec]
derive name = pure []
