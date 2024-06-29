{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}

module Example where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Function ((&))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Monoid (Sum (..))
import Data.Proxy (Proxy (..))
import Effectful (Eff, (:>), runPureEff)
import Effectful.Writer.Dynamic (Writer, runWriterLocal, tell)
import GHC.TypeLits (KnownNat, type (-))
import Prelude hiding (init)
import Type.Reflection
import Workflow

--------------------------------------------------------------------------------

-- | Foo
data X = X

-- | Bar
data Y = Y Int

-- | Baz
data Z = Z

-- | The `Xyz` workflow
data Xyz f i o where
  -- | The `InitX` trans
  InitX :: Xyz Identity () X
  -- | The `InitY` trans
  InitY :: Int -> Xyz Identity () Y
  -- | The `XToY` trans
  XToY :: Int -> Xyz Identity X Y
  -- | The `YToZ` trans
  YToZ :: Xyz (IO `Compose` (,) Int) Y Z

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
        , description = "The `InitX` trans"
        , input = Nothing
        , output = stateInfo SX
        }
    SInitY ->
      TransitionInfo
        { name = "InitY"
        , description = "The `InitY` trans"
        , input = Nothing
        , output = stateInfo SY
        }
    SXToY ->
      TransitionInfo
        { name = "XToY"
        , description = "The `XToY` trans"
        , input = Just (stateInfo SX)
        , output = stateInfo SY
        }
    SYToZ ->
      TransitionInfo
        { name = "YToZ"
        , description = "The `YToZ` trans"
        , input = Just (stateInfo SY)
        , output = stateInfo SZ
        }

  stateTag' :: forall a b proxy. (Typeable a, Typeable b) => proxy a -> Maybe (StateTag Xyz b)
  stateTag' _ =
    case eqTypeRep (TypeRep @a) (TypeRep @b) of
      Nothing -> Nothing
      Just HRefl
        | Just HRefl <- eqTypeRep (TypeRep @a) (TypeRep @X) -> Just SX
        | Just HRefl <- eqTypeRep (TypeRep @a) (TypeRep @Y) -> Just SY
        | Just HRefl <- eqTypeRep (TypeRep @a) (TypeRep @Z) -> Just SZ
        | otherwise -> Nothing

  transitionTag = \case
    InitX -> SInitX
    InitY _ -> SInitY
    XToY _ -> SXToY
    YToZ -> SYToZ

instance ConcreteWorkflow Xyz where
  transImpl :: Xyz f i o -> i -> f o
  transImpl = \case
    InitX -> \() -> pure X
    InitY n -> \() -> pure (Y n)
    XToY n -> \X -> pure (Y n)
    YToZ -> \(Y n) -> Compose do
      putStrLn "Z!"
      pure (n, Z)

-- Helper functions, totally optional

-- `X` is just data, `State s X` is a _state_ with that data. The latter is
-- correct by construction. In other words, if data is wrapped with `State`, you
-- know it's in a valid state and can only change via valid transitions.

initX :: State Xyz X
initX = runIdentity $ init InitX

initY :: Int -> State Xyz Y
initY n = runIdentity $ init (InitY n)

xToY :: Int -> State Xyz X -> State Xyz Y
xToY n i = runIdentity $ trans (XToY n) i

yToZ :: MonadIO m => State Xyz Y -> m (Int, State Xyz Z)
yToZ i = liftIO $ getCompose $ trans YToZ i

_exampleXyzA :: AbstractState Z
_exampleXyzA =
  let
    x = initA SInitX
    y = transA SXToY x
    z = transA SYToZ y
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

-- Attempting to obtain an `State s Z` with `init` rather than `trans` by
-- creating a new workflow with the same data where that's a legal operation.

data EvilXyz f i o where
  EvilInitZ :: EvilXyz Identity () Z
  EvilZToX :: EvilXyz Identity Z X

instance ConcreteWorkflow EvilXyz where
  transImpl :: EvilXyz f i o -> i -> f o
  transImpl = \case
    EvilInitZ -> \() -> pure Z
    EvilZToX -> \Z -> pure X

-- Doesn't compile if you lie about the workflow type, preventing invalid
-- `init`s or `trans`s (outside the canonical ones defined by the type
-- class).

-- _evilInitZ :: State Xyz Z
_evilInitZ :: State EvilXyz Z
_evilInitZ = runIdentity $ init EvilInitZ

-- _evilZToX :: State Xyz Z -> State Xyz X
_evilZToX :: State EvilXyz Z -> State EvilXyz X
_evilZToX i = runIdentity $ trans EvilZToX i

--------------------------------------------------------------------------------

-- Pretend this is in its own module, and the constructor is not exported.
newtype Box n a = UnsafeBox a

data TimeRelease f i o where
  Lock :: KnownNat n => Proxy n -> a -> TimeRelease Identity () (Box n a)
  Tick :: TimeRelease Identity (Box n a) (Box (n - 1) a)
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

--------------------------------------------------------------------------------

-- TODO: Embed a `TimeRelease` into each `TrafficLight` state, to mimic the time
-- you wait at a traffic light, and demonstrate combining workflows.

data Red = Red

data Yellow = Yellow

data Green = Green

data TrafficLight f i o where
  InitRed :: TrafficLight (Eff es) () Red
  -- Count the number of times you go from red to green
  Go :: Writer (Sum Int) :> es => TrafficLight (Eff es) Red Green
  Slow :: TrafficLight (Eff es) Green Yellow
  Stop :: TrafficLight (Eff es) Yellow Red

instance ConcreteWorkflow TrafficLight where
  transImpl :: TrafficLight f i o -> i -> f o
  transImpl = \case
    InitRed -> \() -> pure Red
    Go -> \Red -> pure Green
    Slow -> \Green -> pure Yellow
    Stop -> \Yellow -> pure Red

initRed :: Eff es (State TrafficLight Red)
initRed = init InitRed

go :: Writer (Sum Int) :> es => State TrafficLight Red -> Eff es (State TrafficLight Green)
go i = do
  -- Type annotation only required here because I'm not using `effectful-plugin`
  -- for better type inference.
  tell (Sum @Int 1)
  trans Go i

slow :: State TrafficLight Green -> Eff es (State TrafficLight Yellow)
slow i = trans Slow i

stop :: State TrafficLight Yellow -> Eff es (State TrafficLight Red)
stop i = trans Stop i

_exampleTrafficLight :: (String, Sum Int)
_exampleTrafficLight = runPureEff . runWriterLocal $ do
  red1 <- initRed
  green <- go red1
  yellow <- slow green
  red2 <- stop yellow
  case (getState red1, getState red2) of
    (Red, Red) -> pure "they're equal!"
