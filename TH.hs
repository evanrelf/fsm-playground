{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}

module TH
  ( derive
  )
where

import Language.Haskell.TH

-- This Template Haskell stuff isn't used yet. Still a work in progress; nothing
-- to see here.

-- TODO: Generate `AbstractWorkflow` instance
derive :: Name -> Q [Dec]
derive name = do
  TyConI (DataD _ _ _ _ _cons _) <- reify name
  -- cons should be `GadtC`s, possibly wrapped in `ForallC`
  pure []
