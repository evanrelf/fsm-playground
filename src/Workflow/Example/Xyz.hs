{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Workflow.Example.Xyz where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Type.Reflection
import Workflow

-- | Foo
data X = X

-- | Bar
data Y = Y Int

-- | Baz
data Z = Z

-- | The `Xyz` workflow
data Xyz f i o where
  -- | The `InitX` transition
  InitX :: Xyz Identity () X
  -- | The `InitY` transition
  InitY :: Int -> Xyz Identity () Y
  -- | The `XToY` transition
  XToY :: Int -> Xyz Identity X Y
  -- | The `YToZ` transition
  YToZ :: Xyz (IO `Compose` (,) Int) Y Z
  -- ^ Shows how you'd perform effects (`IO`) and return an output value
  -- (`(,) Int`).

-- TODO: Derive this `AbstractWorkflow` instance with Template Haskell
instance AbstractWorkflow Xyz where
  data StateTag Xyz s where
    SX :: StateTag Xyz X
    SY :: StateTag Xyz Y
    SZ :: StateTag Xyz Z

  data TransitionTag Xyz i o where
    SInitX :: TransitionTag Xyz () X
    SInitY :: TransitionTag Xyz () Y
    SXToY :: TransitionTag Xyz X Y
    SYToZ :: TransitionTag Xyz Y Z

  states =
    [ SomeStateTag SX
    , SomeStateTag SY
    , SomeStateTag SZ
    ]

  transitions =
    [ SomeTransitionTag SInitX
    , SomeTransitionTag SInitY
    , SomeTransitionTag SXToY
    , SomeTransitionTag SYToZ
    ]

  workflowInfo =
    WorkflowInfo
      { name = "Xyz"
      , description = "The `Xyz` workflow"
      , states = stateInfos @Xyz
      , transitions = transitionInfos @Xyz
      }

  stateInfo = \case
    SX -> StateInfo{ name = "X", description = "Foo" }
    SY -> StateInfo{ name = "Y", description = "Bar" }
    SZ -> StateInfo{ name = "Z", description = "Baz" }

  transitionInfo = \case
    SInitX ->
      TransitionInfo
        { name = "InitX"
        , description = "The `InitX` transition"
        , input = Nothing
        , output = stateInfo SX
        }
    SInitY ->
      TransitionInfo
        { name = "InitY"
        , description = "The `InitY` transition"
        , input = Nothing
        , output = stateInfo SY
        }
    SXToY ->
      TransitionInfo
        { name = "XToY"
        , description = "The `XToY` transition"
        , input = Just (stateInfo SX)
        , output = stateInfo SY
        }
    SYToZ ->
      TransitionInfo
        { name = "YToZ"
        , description = "The `YToZ` transition"
        , input = Just (stateInfo SY)
        , output = stateInfo SZ
        }

  stateTag :: forall a. Typeable a => Maybe (StateTag Xyz a)
  stateTag
    | Just HRefl <- eqTypeRep (TypeRep @a) (TypeRep @X) = Just SX
    | Just HRefl <- eqTypeRep (TypeRep @a) (TypeRep @Y) = Just SY
    | Just HRefl <- eqTypeRep (TypeRep @a) (TypeRep @Z) = Just SZ
    | otherwise = Nothing

  transitionTag = \case
    InitX -> SInitX
    InitY _ -> SInitY
    XToY _ -> SXToY
    YToZ -> SYToZ

instance ConcreteWorkflow Xyz where
  transitionRaw :: Xyz f i o -> i -> f o
  transitionRaw = \case
    InitX -> \() -> pure X
    InitY n -> \() -> pure (Y n)
    XToY n -> \X -> pure (Y n)
    YToZ -> \(Y n) -> Compose do
      putStrLn "Z!"
      pure (n, Z)

-- Helper functions, totally optional

-- `X` is just data, `State w X` is a _state_ with that data. The latter is
-- correct by construction. In other words, if data is wrapped with `State`, you
-- know it's in a valid state and can only change via valid transitions.

initX :: State Xyz X
initX = runIdentity $ initialize InitX

initY :: Int -> State Xyz Y
initY n = runIdentity $ initialize (InitY n)

xToY :: Int -> State Xyz X -> State Xyz Y
xToY n i = runIdentity $ transition (XToY n) i

yToZ :: MonadIO m => State Xyz Y -> m (Int, State Xyz Z)
yToZ i = liftIO $ getCompose $ transition YToZ i

_exampleXyzA :: AbstractState Xyz Z
_exampleXyzA =
  let
    x = abstractInitialize SInitX
    y = abstractTransition SXToY x
    z = abstractTransition SYToZ y
  in
    z

_exampleXyzC :: IO (Int, State Xyz Z)
_exampleXyzC =
  let
    x = initX
    y = xToY 42 x
    z = yToZ y
  in
    z

--------------------------------------------------------------------------------

-- Attempting to obtain an `State w Z` with `initialize` rather than `transition`
-- by creating a new workflow with the same data where that's a legal operation.

data EvilXyz f i o where
  EvilInitZ :: EvilXyz Identity () Z
  EvilZToX :: EvilXyz Identity Z X

instance ConcreteWorkflow EvilXyz where
  transitionRaw :: EvilXyz f i o -> i -> f o
  transitionRaw = \case
    EvilInitZ -> \() -> pure Z
    EvilZToX -> \Z -> pure X

-- Doesn't compile if you lie about the workflow type, preventing invalid
-- `initialize`s or `transition`s (outside the canonical ones defined by the
-- type class).

-- _evilInitZ :: State Xyz Z
_evilInitZ :: State EvilXyz Z
_evilInitZ = runIdentity $ initialize EvilInitZ

-- _evilZToX :: State Xyz Z -> State Xyz X
_evilZToX :: State EvilXyz Z -> State EvilXyz X
_evilZToX i = runIdentity $ transition EvilZToX i
