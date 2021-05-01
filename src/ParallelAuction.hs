{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ParallelAuction where

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
import Ledger.AddressMap (UtxoMap)
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

data Bid = Bid
        { bBid    :: Ada
        , bBidder :: PubKeyHash
        }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Bid


newtype ParallelAuctionDatum = ParallelAuctionDatum
    { highestBid :: Bid
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''ParallelAuctionDatum


emptyParallelAuctionDatum :: PubKeyHash -> ParallelAuctionDatum
emptyParallelAuctionDatum self = ParallelAuctionDatum (Bid 0 self)


data ParallelAuctionInput = InputClose | InputBid Ada

PlutusTx.unstableMakeIsData ''ParallelAuctionInput


{-# INLINEABLE mkValidator #-}
mkValidator :: ParallelAuctionDatum -> ParallelAuctionInput -> ScriptContext -> Bool
mkValidator _ InputClose _ = True
mkValidator (ParallelAuctionDatum (Bid curBid curPk)) (InputBid newBid) ctx =
    curBid < newBid

data ParallelAuction

instance Scripts.ScriptType ParallelAuction where
  type DatumType ParallelAuction = ParallelAuctionDatum
  type RedeemerType ParallelAuction = ParallelAuctionInput

inst :: Scripts.ScriptInstance ParallelAuction
inst =
  Scripts.validator @ParallelAuction
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @ParallelAuctionDatum @ParallelAuctionInput

validator :: Validator
validator = Scripts.validatorScript inst

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

--
-- Contract
--

data ParallelAuctionError
  = TContractError ContractError
  | TCurrencyError Currency.CurrencyError
  deriving stock (Haskell.Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''ParallelAuctionError

instance Currency.AsCurrencyError ParallelAuctionError where
  _CurrencyError = _TCurrencyError

instance AsContractError ParallelAuctionError where
  _ContractError = _TContractError

type ParallelAuctionSchema =
  BlockchainActions
    .\/ Endpoint "start" ()
    .\/ Endpoint "bid" Integer
    .\/ Endpoint "close" ()

-- Get utxos

-- TODO
-- - Make sure start can only be called once
-- - Make sure close only closes if done
endpoints :: Contract () ParallelAuctionSchema ParallelAuctionError ()
endpoints = (start' `select` bid' `select` close') >> endpoints
  where
    start' = endpoint @"start" >> start
    bid' = endpoint @"bid" >>= bid
    close' = endpoint @"close" >> close

threadCount :: Int
threadCount = 3

start :: Contract w ParallelAuctionSchema ParallelAuctionError ()
start = do
  ownPk <- ownPubKey
  let ownPkHash = pubKeyHash ownPk
  -- Forge tokens for auction threads
  c :: Currency.OneShotCurrency <-
    Currency.forgeContract (pubKeyHash ownPk) [("auction-theads", fromIntegral threadCount)]
  let cV = Currency.forgedValue c
      -- Create one UTxO for every single value
      cVs = splitToSingleValues cV
      txConstrs = createConstraintsForValues ownPkHash cVs
  logInfo @String $ "submit for tx confirmation"
  -- Submit tx
  ledgerTx <- submitTxConstraints inst txConstrs
  logInfo @String $ printf "wait for tx confirmation"
  void . awaitTxConfirmed . txId $ ledgerTx
  -- Verify current state
  utxoMap <- utxoAt scrAddress
  logInfo @String . printf $ "started, utxo map : " <> show (size utxoMap) -- <> ", " <> show (keys utxoMap)
  where
    createConstraintForValue :: PubKeyHash -> Value -> TxConstraints ParallelAuctionInput ParallelAuctionDatum
    createConstraintForValue self = mustPayToTheScript (emptyParallelAuctionDatum self)
    createConstraintsForValues :: PubKeyHash -> [Value] -> TxConstraints ParallelAuctionInput ParallelAuctionDatum
    createConstraintsForValues self = mconcat . fmap (createConstraintForValue self)

splitToSingleValues :: Value -> [Value]
splitToSingleValues v = do
  (s, tn, amt) <- Value.flattenValue v
  -- TODO: Unsafe?
  replicate (fromIntegral amt) $ Value.singleton s tn 1

bid :: Integer -> Contract w ParallelAuctionSchema ParallelAuctionError ()
bid b = do
  ownPk <- ownPubKey
  let ownPkHash = pubKeyHash ownPk
      adaBid = Ada.Lovelace b
      inputBid = InputBid adaBid
      outputBid = ParallelAuctionDatum $ Bid adaBid ownPkHash
  logI' "Placing bid" [("pk", show ownPk), ("bid", show b)]
  -- Verify current state
  -- TODO Filter out invalid/malicious utxos; how to know which token?
  utxoMap <- utxoAt scrAddress
  logUTxOSize Nothing utxoMap
  -- Select an UTxOs by using public key hash and thread count
  let pkHash = pubKeyHash ownPk
      utxoIndex :: Int = hash pkHash `mod` threadCount
      -- Ensure utxoMap has the same amount as `threadCount`
      utxoToBid@(utxoToBidRef, TxOutTx _ (TxOut addr threadToken _)) = utxoIndex `elemAt` utxoMap

  logI'' "Choosing UTxO" "index" $ show utxoIndex
  let datums :: [ParallelAuctionDatum] =
          utxoMap ^.. folded
          . Control.Lens.to txOutTxDatum
          . _Just
          . Control.Lens.to (\(Datum d) -> PlutusTx.fromData @ParallelAuctionDatum d)
          . _Just
  logI'' "UTxO datums" "datums" $ show datums
  let lookups =
        Constraints.unspentOutputs utxoMap
          <> Constraints.scriptInstanceLookups inst
      -- Built constrainton our own
      inputConstraints = [InputConstraint {icRedeemer = inputBid, icTxOutRef = utxoToBidRef}]
      -- FIXME: Datum should be the updated one from the existing tx
      outputConstraints = [OutputConstraint {ocDatum = outputBid, ocValue = threadToken}]
      txConstrs =
        TxConstraints
          { txConstraints = [],
            txOwnInputs = inputConstraints,
            txOwnOutputs = outputConstraints
          }
  -- Submit tx
  ledgerTx <- submitTxConstraintsWith lookups txConstrs
  logI "Waiting for tx confirmation"
  void . awaitTxConfirmed . txId $ ledgerTx

  -- Print UTxO
  utxoMap <- utxoAt scrAddress
  logInfo @String . printf $ "after bid, utxo map : " <> show (size utxoMap) -- <> ", " <> show (keys utxoMap)
  let datums :: [ParallelAuctionDatum] =
          utxoMap ^.. folded
          . Control.Lens.to txOutTxDatum
          . _Just
          . Control.Lens.to (\(Datum d) -> PlutusTx.fromData @ParallelAuctionDatum d)
          . _Just
  PlutusTx.Prelude.mapM (logI'' "UTxO datums" "datums") $ fmap show datums

  -- TODO How to know which token?
  -- Use this UTxO to do the bidding
  pure ()

close :: Contract w ParallelAuctionSchema ParallelAuctionError ()
close = do
  logI "Closing auction"

-- Helper
logI :: String -> Contract w s e ()
logI = logInfo @String

-- TODO: Replace value tuples with HList.
logI' :: String -> [(String, String)] -> Contract w s e ()
logI' t m = logInfo @String $ t <> printKeyValues m
  where
    printKeyValues [] = ""
    printKeyValues m = ": " <> mconcat (fmap kvToString m)
    kvToString (k, v) = k <> "=" <> show v

logI'' :: String -> String -> String -> Contract w s e ()
logI'' t k v = logI' t [(k, v)]

logUTxOSize :: Maybe String -> UtxoMap -> Contract w s e ()
logUTxOSize title utxoMap =
  let t = fromMaybe "UTxO size of script" title
   in logI'' t "size" $ show (size utxoMap)
