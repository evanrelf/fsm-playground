{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fplugin=Effectful.Plugin #-}

module Workflow.Example.Atm where

import Control.Exception (Exception, throwIO)
import Data.Function ((&))
import Data.Functor.Compose (Compose (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text (pack)
import Data.Text.Encoding qualified as Text (encodeUtf8)
import Effectful
import Effectful.Console.ByteString (Console, runConsole)
import Effectful.Console.ByteString qualified as Console (putStrLn)
import Effectful.Dispatch.Dynamic
import Effectful.Error.Dynamic (Error, throwError, runErrorNoCallStackWith)
import Effectful.State.Dynamic (runStateShared, get, modifyM)
import Effectful.TH (makeEffect)
import Workflow

newtype Card = Card String
  deriving newtype (Eq, Ord)

newtype Pin = Pin String
  deriving newtype (Eq, Ord)

newtype UserId = UserId String
  deriving newtype (Eq, Ord)

newtype Dollars = Dollars Word
  deriving newtype (Show, Eq, Ord, Num)

--------------------------------------------------------------------------------
-- BANK EFFECT
--
-- Models some basic bank-y operations as an effect with the `effectful` effect
-- system library.
--
-- Only an in-memory interpretation is provided here (`runBank`) but you could
-- imagine this effect being interpreted in other ways, e.g. making API calls or
-- talking to a database.
--------------------------------------------------------------------------------

-- Not secure, just a toy example :-)
data BankState = BankState
  { credentials :: Map (Card, Pin) UserId
  , balances :: Map UserId Dollars
  }

data BankError
  = UnknownUser
  | InsufficientFunds
  deriving stock (Show)
  deriving anyclass (Exception)

data Bank :: Effect where
  -- Prefixing these with `Bank*` to avoid conflicts with the workflow data
  -- constructors below.
  BankAuthenticate :: Card -> Pin -> Bank m (Maybe UserId)
  BankGetBalance :: UserId -> Bank m Dollars
  BankWithdraw :: UserId -> Dollars -> Bank m ()
  BankDeposit :: UserId -> Dollars -> Bank m ()

makeEffect ''Bank

runBank
  :: Error BankError :> es
  => BankState
  -> Eff (Bank : es) a
  -> Eff es (a, BankState)
runBank initialState = reinterpret (runStateShared initialState) \_env -> \case
  BankAuthenticate card pin -> do
    state <- get
    pure $ Map.lookup (card, pin) state.credentials

  BankGetBalance userId -> do
    state <- get
    case Map.lookup userId state.balances of
      Nothing -> throwError UnknownUser
      Just balance -> pure balance

  BankWithdraw userId amount ->
    modifyM \state -> do
      case Map.lookup userId state.balances of
        Nothing ->
          throwError UnknownUser
        Just balance ->
          if amount > balance then
            throwError InsufficientFunds
          else do
            let balances = Map.insert userId (balance - amount) state.balances
            pure state{ balances }

  BankDeposit userId amount ->
    modifyM \state -> do
      case Map.lookup userId state.balances of
        Nothing ->
          throwError UnknownUser
        Just balance -> do
            let balances = Map.insert userId (balance + amount) state.balances
            pure state{ balances }

--------------------------------------------------------------------------------
-- ATM WORKFLOW
--
-- Models some basic interactions you could have with an ATM. Uses the `Bank`
-- effect defined above to allow reinterpretation in a testing context (or
-- whatever).
--
-- In other words: `AbstractWorkflow` lets you _talk_ about the states and
-- transitions in the abstract - what are they called, which transitions are
-- valid, etc. - but `ConcreteWorkflow` + an effects system lets you _execute
-- programs_ in a (more) abstract way.
--
-- You can write a workflow that works in production with real data launching
-- real nukes, or in a simulation with stubbed out effects + extra logging +
-- extra bookkeeping.
--------------------------------------------------------------------------------

data Off = Off

data AwaitingCard = AwaitingCard

data AwaitingPin = AwaitingPin
  { card :: Card
  }

data Menu = Menu
  { userId :: UserId
  }

-- These might look a little scary, but users don't have to work with this
-- type, they just use the helper functions you define atop these
-- (e.g. `enterPin`).
--
-- If you do need to work with this type, don't fret. There's only a few things
-- you need to know. Let's look at `AtmEnterPin` as an example:
--
-- @
--
--                1             2          3                        4           5
--                |             |          |                        |           |
--                v             v          v                        v           v
--                ~~~~~~~~~~    ~~~        ~~~~~~~~~~~~~~~~~~~~~~~~ ~~~~~~~~~~~ ~~~~
-- AtmEnterPin :: Bank :> es => Pin -> Atm (Eff es `Compose` Maybe) AwaitingPin Menu
-- @
--
-- First, an overview:
--
-- * We're starting in the `AwaitingPin` state (4).
-- * We're receiving a `Pin` as input (2).
-- * We're transitioning to the `Menu` state (5).
-- * While transitioning, we're allowed to perform `Bank` effects (we need
--   to provide the card and pin to authenticate with the bank) (1 and 3).
-- * We may fail to transition to the `Menu` state (the card and/or pin may
--   be incorrect), so the result is wrapped in a `Maybe` (3).
--
-- Now in greater depth:
--
-- 1. These are constraints.
--
--    If you have any type parameters that aren't pinned to a concrete
--    type (e.g. `Menu` is concrete but `es` is polymorphic), you can add
--    constraints to limit which types are acceptable.
--
--    But constraints are liberating. By knowing that the list of effects `es`
--    contains the `Bank` effect, we know that the `AtmEnterPin` transition
--    has access to `Bank` stuff, e.g. `BankAuthenticate`. We'll want to
--    authenticate with the pin we receive as input.
--
-- 2. This is an input.
--
--    It's saying "in addition to the current state, I also need this extra
--    piece of information in order to perform this transition". So in this
--    case, to perform the `AtmEnterPin` transition, we need the `Pin`.
--
-- 3. This is the context in which the transition is performed.
--
--    This is how you can perform effects, or return additional outputs
--    (besides the new state). The only requirement for this type is that
--    it's a functor.
--
--      * If you don't need to perform any effects or return any extra
--        information, you should choose `Identity`. It's a simple wrapper that
--        does nothing and is trivially unwrapped.
--
--      * If you want to return an output in addition to your state, you should
--        choose `(,) a` (a tuple) or something equivalent. The syntax there is
--        weird, so just think of the result you'll get from the transition as
--        `(a, s)`, where `a` is your extra data, and `s` is your new state.
--
--      * If you want to check a dynamic condition (something at runtime) and
--        only transition if that condition is satisfied, you could choose
--        `Either e` or `Maybe`.
--
--      * If you want to perform some effects, there's a number of types you
--        could choose. Some examples are `IO`, `State`, `Writer`, `ST s`, etc.
--
--      * If you want some combination of the above, you'll want `Compose`. It
--        allows you to apply multiple wrappers. For example,
--        @IO `Compose` Maybe@ will result in `IO (Maybe (State Atm s))` (where
--        `s` is the type of the next state after performing the transition).
--
-- 4. This is the input/initial state.
--
-- 5. This is the output/final state.
--
--    This is what's wrapped in the type we discussed in #3 above.
data Atm f i o where
  -- Prefixing these with `Atm*` to avoid conflicts with the effect data
  -- constructors above.
  AtmNew :: Atm (Eff es) () Off
  AtmPowerOn :: Atm (Eff es) Off AwaitingCard
  AtmInsertCard :: Card -> Atm (Eff es) AwaitingCard AwaitingPin
  AtmEnterPin :: Bank :> es => Pin -> Atm (Eff es `Compose` Maybe) AwaitingPin Menu
  AtmWithdraw :: Bank :> es => Dollars -> Atm (Eff es) Menu Menu
  AtmDeposit :: Bank :> es => Dollars -> Atm (Eff es) Menu Menu
  AtmGetBalance :: Bank :> es => Atm (Eff es `Compose` ((,) Dollars)) Menu Menu
  AtmExit :: Atm (Eff es) Menu AwaitingCard
  AtmPowerOff :: Atm (Eff es) i Off

instance ConcreteWorkflow Atm where
  transitionRaw = \case
    AtmNew -> \() -> do
      pure Off

    AtmPowerOn -> \Off -> do
      pure AwaitingCard

    AtmInsertCard card -> \AwaitingCard -> do
      pure (AwaitingPin card)

    AtmEnterPin pin -> \(AwaitingPin card) -> Compose do
      mUserId <- bankAuthenticate card pin
      case mUserId of
        Nothing -> pure Nothing
        Just userId -> pure $ Just (Menu userId)

    AtmWithdraw amount -> \menu@(Menu userId) -> do
      bankWithdraw userId amount
      pure menu

    AtmDeposit amount -> \menu@(Menu userId) -> do
      bankDeposit userId amount
      pure menu

    AtmGetBalance -> \menu@(Menu userId) -> Compose do
      balance <- bankGetBalance userId
      pure (balance, menu)

    AtmExit -> \(Menu _) -> do
      pure AwaitingCard

    AtmPowerOff -> \_ -> do
      pure Off

-- TODO: Derive `AbstractWorkflow` instance with Template Haskell

new :: Eff es (State Atm Off)
new = initialize AtmNew

powerOn :: State Atm Off -> Eff es (State Atm AwaitingCard)
powerOn state = transition AtmPowerOn state

insertCard :: Card -> State Atm AwaitingCard -> Eff es (State Atm AwaitingPin)
insertCard card state = transition (AtmInsertCard card) state

enterPin :: Bank :> es => Pin -> State Atm AwaitingPin -> Eff es (Maybe (State Atm Menu))
enterPin pin state = getCompose $ transition (AtmEnterPin pin) state

withdraw :: Bank :> es => Dollars -> State Atm Menu -> Eff es (State Atm Menu)
withdraw amount state = transition (AtmWithdraw amount) state

deposit :: Bank :> es => Dollars -> State Atm Menu -> Eff es (State Atm Menu)
deposit amount state = transition (AtmDeposit amount) state

getBalance :: Bank :> es => State Atm Menu -> Eff es (Dollars, State Atm Menu)
getBalance state = getCompose $ transition AtmGetBalance state

exit :: State Atm Menu -> Eff es (State Atm AwaitingCard)
exit state = transition AtmExit state

powerOff :: State Atm state -> Eff es (State Atm Off)
powerOff state = transition AtmPowerOff state

--------------------------------------------------------------------------------
-- DEMO
--------------------------------------------------------------------------------

demo :: IO ()
demo = do
  let bankState =
        BankState
          { credentials = Map.singleton (Card "card", Pin "pin") (UserId "evan")
          , balances = Map.singleton (UserId "evan") 200
          }

  program
    & runBank bankState
    & fmap (\(result, _finalBankState) -> result)
    & runErrorNoCallStackWith (liftIO . throwIO)
    & runConsole
    & runEff
    -- Like `void`, but asserting that we end in the `Off` state.
    & fmap (\(State Off) -> ())

program :: (Bank :> es, Console :> es) => Eff es (State Atm Off)
program = do
  -- Type annotations aren't necessary here for type inference, just including
  -- to show how the state changes over time.

  atm :: State Atm Off <- new
  atm :: State Atm AwaitingCard <- powerOn atm
  atm :: State Atm AwaitingPin <- insertCard (Card "card") atm
  mAtm :: Maybe (State Atm Menu) <- enterPin (Pin "pin") atm

  -- We'll discharge the `Nothing` here to keep the example simple. You'd wanna
  -- handle this properly in real programs, of course.
  let atm = fromMaybe (error "uh oh") mAtm

  (balance, atm :: State Atm Menu) <- getBalance atm
  printBytes balance

  atm :: State Atm Menu <- withdraw 100 atm
  (balance, atm :: State Atm Menu) <- getBalance atm
  printBytes balance

  atm :: State Atm Menu <- deposit 300 atm
  (balance, atm :: State Atm Menu) <- getBalance atm
  printBytes balance

  powerOff atm

printBytes :: (Show a, Console :> es) => a -> Eff es ()
printBytes a =
  a & show
    & Text.pack
    & Text.encodeUtf8
    & Console.putStrLn
