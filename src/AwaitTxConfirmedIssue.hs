{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AwaitTxConfirmedIssue where

-- Issue brought up by @dino in discord.
-- - `awaitTxConfirmed` loops forever if validation fails
-- - I.e. contract is blocked for further requests
-- - Same as what happened in lecture 02
--
-- Solution:
-- - Add a timeout to the `awaitTxConfirmed`
-- - Note: Transaction confirmation handling will change in future, so probably not worth to contribute
--

import Control.Monad hiding (fmap)
import Data.Text (Text)
import Data.Void (Void)
import Ledger
    ( ScriptContext,
      scriptCurrencySymbol,
      mkMonetaryPolicyScript,
      txId,
      Slot,
      CurrencySymbol )
import Ledger.Constraints as Constraints
  ( monetaryPolicy,
    mustForgeValue,
  )
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value as Value (singleton)
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import Plutus.Contract as Contract
  ( AsContractError,
    BlockchainActions,
    Contract,
    Endpoint,
    HasBlockchainActions,
    awaitTxConfirmed,
    currentSlot,
    endpoint,
    logError,
    logInfo,
    submitTxConstraintsWith,
    throwError,
    timeout,
    type (.\/),
  )
import Plutus.Trace.Emulator as Emulator
  ( activateContractWallet,
    callEndpoint,
    runEmulatorTraceIO,
    waitNSlots,
  )
import qualified PlutusTx
import PlutusTx.Prelude
  ( AdditiveSemigroup ((+)),
    Bool (False),
    IO,
    Integer,
    Show (show),
    String,
    maybe,
    traceIfFalse,
    ($),
  )
import Text.Printf (printf)
import Wallet.Emulator.Wallet (Wallet (Wallet))

{-# INLINEABLE mkPolicy #-}
mkPolicy :: ScriptContext -> Bool
mkPolicy _ =
  traceIfFalse "Validation error" False

policy :: Scripts.MonetaryPolicy
policy =
  mkMonetaryPolicyScript
    $$(PlutusTx.compile [||Scripts.wrapMonetaryPolicy mkPolicy||])

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy

type SignedSchema =
  BlockchainActions
    .\/ Endpoint "mint" Integer

mint :: Integer -> Contract w SignedSchema Text ()
mint amt = do
  let val = Value.singleton curSymbol "ABC" amt
      lookups = Constraints.monetaryPolicy $ policy
      tx = Constraints.mustForgeValue val
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
  withTimeoutLogging 2 $ do
    Contract.logInfo @String $ printf "Awaiting confirmation"
    awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)

withTimeout ::
  (HasBlockchainActions s, AsContractError e) =>
  Slot ->
  Contract w s e a ->
  Contract w s e a ->
  Contract w s e a
withTimeout t c onError = do
  curSlot <- Contract.currentSlot
  result <- timeout (curSlot + t) c
  maybe onError return result

withTimeoutLogging ::
  HasBlockchainActions s =>
  Slot ->
  Contract w s Text () ->
  Contract w s Text ()
withTimeoutLogging t c = withTimeout t c $ Contract.logError @String $ printf "Timeout for awaiting confirmation"

withTimeoutThrowError ::
  HasBlockchainActions s =>
  Slot ->
  Contract w s Text () ->
  Contract w s Text ()
withTimeoutThrowError t c = withTimeout t c $ Contract.throwError "Timeout for awaiting confirmation"

endpoints :: Contract () SignedSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >>= mint

mkSchemaDefinitions ''SignedSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
  h <- activateContractWallet (Wallet 1) endpoints
  callEndpoint @"mint" h 333
  void $ Emulator.waitNSlots 3
  callEndpoint @"mint" h 444
  void $ Emulator.waitNSlots 3
  callEndpoint @"mint" h 555
  void $ Emulator.waitNSlots 3
