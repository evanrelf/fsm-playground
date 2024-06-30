{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Workflow.Example.TrafficLight where

import Data.Monoid (Sum (..))
import Effectful (Eff, (:>), runPureEff)
import Effectful.Writer.Dynamic (Writer, runWriterLocal, tell)
import Prelude hiding (init)
import Workflow

data Red = Red

data Yellow = Yellow

data Green = Green

data TrafficLight f i o where
  InitRed :: Applicative f => TrafficLight f () Red
  -- Count the number of times you go from red to green
  Go :: Writer (Sum Int) :> es => TrafficLight (Eff es) Red Green
  Slow :: Applicative f => TrafficLight f Green Yellow
  Stop :: Applicative f => TrafficLight f Yellow Red

instance ConcreteWorkflow TrafficLight where
  transImpl :: TrafficLight f i o -> i -> f o
  transImpl = \case
    InitRed -> \() -> pure Red
    -- Type annotation only required here because I'm not using
    -- `effectful-plugin` for better type inference.
    Go -> \Red -> tell (Sum @Int 1) *> pure Green
    Slow -> \Green -> pure Yellow
    Stop -> \Yellow -> pure Red

initRed :: Eff es (State TrafficLight Red)
initRed = init InitRed

go :: Writer (Sum Int) :> es => State TrafficLight Red -> Eff es (State TrafficLight Green)
go i = trans Go i

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
