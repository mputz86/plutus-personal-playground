{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ParallelContract where

import Control.Lens
import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (hash)
import Data.Map as Map
import Data.Text (Text, pack)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import Ledger.Constraints as Constraints
import qualified Ledger.Constraints.OffChain as Constraints
import Ledger.Constraints.TxConstraints (InputConstraint (..), OutputConstraint (..))
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Typed.Tx as Typed
import Playground.Contract (ToSchema)
import Plutus.Contract hiding (when)
import qualified Plutus.Contracts.Currency as Currency
import qualified Plutus.V1.Ledger.Api as Api
import qualified Plutus.V1.Ledger.Scripts (unitDatum, unitRedeemer)
import qualified Plutus.V1.Ledger.Value as Value
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Text.Printf (printf)
import Prelude (Semigroup (..))
import qualified Prelude as Haskell

-- data Config = Config
--     { threadCount :: Int
--     , tokenId :: Int
--     } deriving (Eq, Show)

-- data State = Start | Bid Integer | Close

{-# INLINEABLE mkValidator #-}
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator _ () _ = True

data ParallelContract

instance Scripts.ScriptType ParallelContract where
  type DatumType ParallelContract = ()
  type RedeemerType ParallelContract = ()

inst :: Scripts.ScriptInstance ParallelContract
inst =
  Scripts.validator @ParallelContract
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @() @()

validator :: Validator
validator = Scripts.validatorScript inst

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

type ParallelContractSchema =
  BlockchainActions
    .\/ Endpoint "start" ()
    .\/ Endpoint "bid" Integer

-- .\/ Endpoint "close" ()

data ParallelContractError
  = TContractError ContractError
  | TCurrencyError Currency.CurrencyError
  deriving stock (Haskell.Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''ParallelContractError

instance Currency.AsCurrencyError ParallelContractError where
  _CurrencyError = _TCurrencyError

instance AsContractError ParallelContractError where
  _ContractError = _TContractError

threadCount :: Int
threadCount = 10

start :: Contract w ParallelContractSchema ParallelContractError ()
start = do
  ownPk <- ownPubKey
  -- Forge tokens for auction threads
  c :: Currency.OneShotCurrency <-
    Currency.forgeContract (pubKeyHash ownPk) [("auction-theads", fromIntegral threadCount)]
  let cV = Currency.forgedValue c
      -- Create one UTxO for every single value
      cVs = splitToSingleValues cV
      txConstrs = createConstraintsForValues cVs
  logInfo @String $ printf "submit for tx confirmation"
  -- Submit tx
  ledgerTx <- submitTxConstraints inst txConstrs
  logInfo @String $ printf "wait for tx confirmation"
  void . awaitTxConfirmed . txId $ ledgerTx
  -- Verify current state
  utxoMap <- utxoAt scrAddress
  logInfo @String . printf $ "started, utxo map : " <> show (size utxoMap) -- <> ", " <> show (keys utxoMap)
  where
    createConstraintForValue :: Value -> TxConstraints () ()
    createConstraintForValue = mustPayToTheScript ()
    createConstraintsForValues :: [Value] -> TxConstraints () ()
    createConstraintsForValues = mconcat . fmap createConstraintForValue

splitToSingleValues :: Value -> [Value]
splitToSingleValues v = do
  (s, tn, amt) <- Value.flattenValue v
  -- TODO: Unsafe?
  replicate (fromIntegral amt) $ Value.singleton s tn 1

bid :: Integer -> Contract w ParallelContractSchema ParallelContractError ()
bid b = do
  ownPk <- ownPubKey
  logInfo @String . printf $ "bidding " <> show b
  -- Verify current state
  -- TODO Filter out invalid/malicious utxos; how to know which token?
  utxoMap <- utxoAt scrAddress
  logInfo @String . printf $ "utxo map : " <> show (size utxoMap) <> ", " <> show (keys utxoMap)
  -- Select an UTxOs by using public key hash and thread count
  let pkHash = pubKeyHash ownPk
      utxoIndex :: Int = hash pkHash `mod` threadCount
      -- Ensure utxoMap has the same amount as `threadCount`
      utxoToBid@(utxoToBidRef, TxOutTx _ (TxOut addr threadToken _)) = utxoIndex `elemAt` utxoMap
  logInfo @String $ printf "Choosing UTxO number " <> show utxoIndex
  let lookups =
        Constraints.unspentOutputs utxoMap
          <> Constraints.scriptInstanceLookups inst
      -- Built constrainton our own
      inputConstraints = [InputConstraint {icRedeemer = (), icTxOutRef = utxoToBidRef}]
      outputConstraints = [OutputConstraint {ocDatum = (), ocValue = threadToken}]
      txConstrs =
        TxConstraints
          { txConstraints = [],
            txOwnInputs = inputConstraints,
            txOwnOutputs = outputConstraints
          }
  -- Submit tx
  ledgerTx <- submitTxConstraintsWith lookups txConstrs
  logInfo @String $ printf "wait for tx confirmation"
  void . awaitTxConfirmed . txId $ ledgerTx

  -- Print utxo
  utxoMap <- utxoAt scrAddress
  logInfo @String . printf $ "after bid, utxo map : " <> show (size utxoMap) -- <> ", " <> show (keys utxoMap)

  -- TODO How to know which token?
  -- Use this UTxO to do the bidding
  pure ()

close :: Contract w ParallelContractSchema ParallelContractError ()
close = do
  ownPk <- ownPubKey
  logInfo @String . printf $ "closing " <> show ownPk

-- Get utxos

-- TODO
-- - Make sure start can only be called once
-- - Make sure close only closes if done
endpoints :: Contract () ParallelContractSchema ParallelContractError ()
endpoints = (start' `select` bid') >> endpoints
  where
    start' = endpoint @"start" >> start
    bid' = endpoint @"bid" >>= bid

-- close' = endpoint @"close" >> close
