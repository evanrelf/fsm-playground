{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fplugin=Effectful.Plugin #-}

module Workflow.Example.Atm where

import Data.Functor.Compose (Compose (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Dynamic (Error, throwError)
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

data BankState = BankState
  { credentials :: Map (Card, Pin) UserId
  , balances :: Map UserId Word
  }

data BankError
  = InsufficientFunds
  | UnknownUser

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
      Just balance' -> pure balance'

  BankWithdraw userId amount ->
    modifyM \state -> do
      case Map.lookup userId state.balances of
        Nothing ->
          throwError UnknownUser
        Just balance' ->
          if amount > balance' then
            throwError InsufficientFunds
          else do
            let balances = Map.insert userId (balance' - amount) state.balances
            pure state{ balances }

  BankDeposit userId amount ->
    modifyM \state -> do
      case Map.lookup userId state.balances of
        Nothing ->
          throwError UnknownUser
        Just balance' -> do
            let balances = Map.insert userId (balance' + amount) state.balances
            pure state{ balances }

exampleBankState :: BankState
exampleBankState =
  BankState
    { credentials = Map.singleton ("card", "pin") "evan"
    , balances = Map.singleton "evan" 200
    }

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
  AtmBalance :: Bank :> es => Atm (Eff es `Compose` ((,) Word)) Menu Menu
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

    AtmBalance -> \menu@(Menu userId) -> Compose do
      balance' <- bankGetBalance userId
      pure (balance', menu)

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

balance :: Bank :> es => State Atm Menu -> Eff es (Word, State Atm Menu)
balance state = getCompose $ trans AtmBalance state

exit :: State Atm Menu -> Eff es (State Atm AwaitingCard)
exit state = trans AtmExit state

powerOff :: State Atm state -> Eff es (State Atm Off)
powerOff state = trans AtmPowerOff state
