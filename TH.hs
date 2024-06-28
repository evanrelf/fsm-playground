{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module TH
  ( derive
  )
where

import Data.Kind (Type)
import GHC.TypeError (ErrorMessage (..), TypeError)
import Language.Haskell.TH hiding (Type)

-- This Template Haskell stuff isn't used yet. Still a work in progress; nothing
-- to see here.

-- TODO: Generate `AbstractWorkflow` instance
derive :: Name -> Q [Dec]
derive name = pure []
