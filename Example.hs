{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}

module Example where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Function ((&))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Proxy (Proxy (..))
import Data.Void (Void)
import GHC.TypeLits (KnownNat, type (-))
import Lib
import Prelude hiding (init)

--------------------------------------------------------------------------------

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
  YToZ :: Xyz IO Y Z


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

  workflowInfo = WorkflowInfo "Xyz" "The `Xyz` workflow"

  stateInfo = \case
    SX -> StateInfo "X" "Foo"
    SY -> StateInfo "Y" "Bar"
    SZ -> StateInfo "Z" "Baz"

  transitionInfo = \case
    SInitX -> TransitionInfo "InitX" "The `InitX` transition" TransitionKind_Init
    SInitY -> TransitionInfo "InitY" "The `InitY` transition" TransitionKind_Init
    SXToY -> TransitionInfo "XToY" "The `XToY` transition" TransitionKind_Transition
    SYToZ -> TransitionInfo "YToZ" "The `YToZ` transition" TransitionKind_Transition

instance Workflow Xyz where
  transitionRaw :: Xyz f i o -> i -> f o
  transitionRaw = \case
    InitX -> \() -> pure X
    InitY n -> \() -> pure (Y n)
    XToY n -> \X -> pure (Y n)
    YToZ -> \(Y _) -> putStrLn "Z!" *> pure Z

-- Helper functions, totally optional

-- `X` is just data, `State s X` is a _state_ with that data. The latter is
-- correct by construction. In other words, if data is wrapped with `State`, you
-- know it's in a valid state and can only change via valid transitions.

initX :: State Xyz X
initX = runIdentity $ init InitX

initY :: Int -> State Xyz Y
initY n = runIdentity $ init (InitY n)

xToY :: Int -> State Xyz X -> State Xyz Y
xToY n i = runIdentity $ transition (XToY n) i

yToZ :: MonadIO m => State Xyz Y -> m (State Xyz Z)
yToZ i = liftIO $ transition YToZ i

_exampleXyz :: IO (State Xyz Z)
_exampleXyz =
  let
    x = initX
    y = xToY 42 x
    z = yToZ y
  in
    z

--------------------------------------------------------------------------------

-- Attempting to obtain an `State s Z` with `init` rather than `transition` by
-- creating a new workflow with the same data where that's a legal operation.

data EvilXyz f i o where
  EvilInitZ :: EvilXyz Identity () Z
  EvilZToX :: EvilXyz Identity Z X

instance Workflow EvilXyz where
  transitionRaw :: EvilXyz f i o -> i -> f o
  transitionRaw = \case
    EvilInitZ -> \() -> pure Z
    EvilZToX -> \Z -> pure X

-- Doesn't compile if you lie about the workflow type, preventing invalid
-- `init`s or `transition`s (outside the canonical ones defined by the type
-- class).

-- _evilInitZ :: State Xyz Z
_evilInitZ :: State EvilXyz Z
_evilInitZ = runIdentity $ init EvilInitZ

-- _evilZToX :: State Xyz Z -> State Xyz X
_evilZToX :: State EvilXyz Z -> State EvilXyz X
_evilZToX i = runIdentity $ transition EvilZToX i

--------------------------------------------------------------------------------

-- Pretend this is in its own module, and the constructor is not exported.
newtype Box n a = UnsafeBox a

data TimeRelease f i o where
  Lock :: KnownNat n => Proxy n -> a -> TimeRelease Identity () (Box n a)
  Tick :: TimeRelease Identity (Box n a) (Box (n - 1) a)
  -- Using `Const` as the functor to escape the workflow and return a plain
  -- value (not something wrapped in `State`).
  --
  -- `Void` is just for good measure; it's driving the point home that the next
  -- state will never be reached, because we return a value instead.
  Unlock :: TimeRelease (Const a) (Box 0 a) Void

instance Workflow TimeRelease where
  transitionRaw :: TimeRelease f i o -> i -> f o
  transitionRaw = \case
    Lock _ a -> \() -> pure (UnsafeBox a)
    Tick -> \(UnsafeBox a) -> pure (UnsafeBox a)
    Unlock -> \(UnsafeBox a) -> Const a

lock :: forall n a. KnownNat n => a -> State TimeRelease (Box n a)
lock a = runIdentity $ init (Lock (Proxy @n) a)

tick :: State TimeRelease (Box n a) -> State TimeRelease (Box (n - 1) a)
tick i = runIdentity $ transition Tick i

unlock :: State TimeRelease (Box 0 a) -> a
unlock i = getConst $ transition Unlock i

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
  InitRed :: TrafficLight Identity () Red
  Go :: TrafficLight Identity Red Green
  Slow :: TrafficLight Identity Green Yellow
  Stop :: TrafficLight Identity Yellow Red
  -- Here's another use of `Const` that allows inspecting the plain value
  -- wrapped in `State` at _any point_ in the life of this workflow.
  --
  -- You can think of this as a `getState :: State s a -> a` function, or
  -- pattern matching on the `State` data constructor, but limited to the
  -- `TrafficLight` workflow.
  Inspect :: TrafficLight (Const i) i Void

instance Workflow TrafficLight where
  transitionRaw :: TrafficLight f i o -> i -> f o
  transitionRaw = \case
    InitRed -> \() -> pure Red
    Go -> \Red -> pure Green
    Slow -> \Green -> pure Yellow
    Stop -> \Yellow -> pure Red
    Inspect -> \i -> Const i

initRed :: State TrafficLight Red
initRed = runIdentity $ init InitRed

go :: State TrafficLight Red -> State TrafficLight Green
go i = runIdentity $ transition Go i

slow :: State TrafficLight Green -> State TrafficLight Yellow
slow i = runIdentity $ transition Slow i

stop :: State TrafficLight Yellow -> State TrafficLight Red
stop i = runIdentity $ transition Stop i

inspect :: State TrafficLight a -> a
inspect i = getConst $ transition Inspect i

_exampleTrafficLight :: String
_exampleTrafficLight =
  let
    red1 = initRed
    green = go red1
    yellow = slow green
    red2 = stop yellow
  in
    case (inspect red1, inspect red2) of (Red, Red) -> "they're equal!"
