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
import Prelude hiding (init)
import Workflow

--------------------------------------------------------------------------------
-- BANK EFFECT
--------------------------------------------------------------------------------

type Card = String

type Pin = String

type UserId = String

type Dollars = Word

-- Not secure, just a toy example :-)
data BankState = BankState
  { credentials :: Map (Card, Pin) UserId
  , balances :: Map UserId Word
  }

data BankError
  = UnknownUser
  | InsufficientFunds
  deriving stock (Show)
  deriving anyclass (Exception)

data Bank :: Effect where
  BankAuthenticate :: Card -> Pin -> Bank m (Maybe UserId)
  BankGetBalance :: UserId -> Bank m Word
  BankWithdraw :: UserId -> Word -> Bank m ()
  BankDeposit :: UserId -> Word -> Bank m ()

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
--------------------------------------------------------------------------------

data Off = Off

data AwaitingCard = AwaitingCard

data AwaitingPin = AwaitingPin Card

data Menu = Menu UserId

data Atm f i o where
  AtmNew :: Atm (Eff es) () Off
  AtmPowerOn :: Atm (Eff es) Off AwaitingCard
  AtmInsertCard :: Card -> Atm (Eff es) AwaitingCard AwaitingPin
  AtmEnterPin :: Bank :> es => Card -> Atm (Eff es `Compose` Maybe) AwaitingPin Menu
  AtmWithdraw :: Bank :> es => Word -> Atm (Eff es) Menu Menu
  AtmDeposit :: Bank :> es => Word -> Atm (Eff es) Menu Menu
  AtmGetBalance :: Bank :> es => Atm (Eff es `Compose` ((,) Word)) Menu Menu
  AtmExit :: Atm (Eff es) Menu AwaitingCard
  AtmPowerOff :: Atm (Eff es) i Off

instance ConcreteWorkflow Atm where
  transImpl = \case
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


new :: Eff es (State Atm Off)
new = init AtmNew

powerOn :: State Atm Off -> Eff es (State Atm AwaitingCard)
powerOn state = trans AtmPowerOn state

insertCard :: Card -> State Atm AwaitingCard -> Eff es (State Atm AwaitingPin)
insertCard card state = trans (AtmInsertCard card) state

enterPin :: Bank :> es => Pin -> State Atm AwaitingPin -> Eff es (Maybe (State Atm Menu))
enterPin pin state = getCompose $ trans (AtmEnterPin pin) state

withdraw :: Bank :> es => Word -> State Atm Menu -> Eff es (State Atm Menu)
withdraw amount state = trans (AtmWithdraw amount) state

deposit :: Bank :> es => Word -> State Atm Menu -> Eff es (State Atm Menu)
deposit amount state = trans (AtmDeposit amount) state

getBalance :: Bank :> es => State Atm Menu -> Eff es (Word, State Atm Menu)
getBalance state = getCompose $ trans AtmGetBalance state

exit :: State Atm Menu -> Eff es (State Atm AwaitingCard)
exit state = trans AtmExit state

powerOff :: State Atm state -> Eff es (State Atm Off)
powerOff state = trans AtmPowerOff state

--------------------------------------------------------------------------------
-- DEMO
--------------------------------------------------------------------------------

demo :: IO ()
demo = do
  let bankState =
        BankState
          { credentials = Map.singleton ("card", "pin") "evan"
          , balances = Map.singleton "evan" 200
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
  atm :: State Atm AwaitingPin <- insertCard "card" atm
  mAtm :: Maybe (State Atm Menu) <- enterPin "pin" atm

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
