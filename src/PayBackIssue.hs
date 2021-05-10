{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PayBackIssue where

import Control.Lens
import Control.Monad hiding (fmap)
import qualified Control.Monad.Freer.Extras as Extras
import Data.Aeson (FromJSON, ToJSON)
import Data.Map as Map
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import Ledger.AddressMap (UtxoMap)
import Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Plutus.Contract hiding (when)
-- FIXME HLS fix: Comment for HLS to work
-- import Plutus.Contracts.Currency (CurrencyError, forgeContract, forgedValue)
import qualified Plutus.Trace.Emulator as Emulator
import Plutus.V1.Ledger.Value
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx.Prelude hiding (Semigroup (..), (==))
import qualified Wallet.Emulator.Wallet as Wallet
import Prelude (Semigroup (..))
import qualified Prelude as Haskell

data Bid = Bid
  { bBid :: Ada,
    bBidder :: PubKeyHash
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Bid

newtype PayBackDatum = PayBackDatum Bid
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''PayBackDatum

newtype PayBackInput = PayBackInput Bid

PlutusTx.unstableMakeIsData ''PayBackInput

{-# INLINEABLE mkValidator #-}
mkValidator :: PayBackDatum -> PayBackInput -> ScriptContext -> Bool
mkValidator _ _ _ = True

data PayBack

instance Scripts.ScriptType PayBack where
  type DatumType PayBack = PayBackDatum
  type RedeemerType PayBack = PayBackInput

inst :: Scripts.ScriptInstance PayBack
inst =
  Scripts.validator @PayBack
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @PayBackDatum @PayBackInput

validator :: Validator
validator = Scripts.validatorScript inst

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

-- | Contract errors
data PayBackError
  = TContractError ContractError
  -- FIXME HLS fix: Comment for HLS to work
  -- | TCurrencyError CurrencyError
  deriving stock (Haskell.Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''PayBackError

-- FIXME HLS fix: Comment for HLS to work
-- instance AsCurrencyError PayBackError where
--   _CurrencyError = _TCurrencyError

instance AsContractError PayBackError where
  _ContractError = _TContractError

type PayBackSchema =
  BlockchainActions
    .\/ Endpoint "start" ()
    .\/ Endpoint "bid" Integer

-- Simplify type for auction contract.
type PayBackContract a = Contract () PayBackSchema PayBackError a

-- | Contract endpoints
start :: PayBackContract ()
start = do
  ownPk <- ownPubKey
  let ownPkHash = pubKeyHash ownPk
      scrInst = inst
      scr = validator
  -- FIXME HLS fix: Comment for HLS to work
  -- tokenCurrency <- forgeContract ownPkHash [("tracing-token", 1)]
  -- let tokenValue = forgedValue tokenCurrency
  -- FIXME HLS fix: Uncomment for HLS to work
  let tokenValue = undefined
  utxoMap <- utxoAt scrAddress
  let lookups =
        Constraints.unspentOutputs utxoMap
          <> Constraints.scriptInstanceLookups scrInst
          <> Constraints.otherScript scr
      placeInitialBid =
        mustPayToTheScript (PayBackDatum $ Bid 0 ownPkHash) tokenValue
      constraints = placeInitialBid
  logI "Starting bidding"
  ledgerTx <- submitTxConstraintsWith lookups constraints
  void . awaitTxConfirmed . txId $ ledgerTx
  printUTxODatums scrAddress

bid :: Integer -> PayBackContract ()
bid b = do
  ownPk <- ownPubKey
  let ownPkHash = pubKeyHash ownPk
      scrInst = inst
      scr = validator
      ownBid = Bid (Ada.Lovelace b) ownPkHash
  logI' "Trying to place bid" [("pk", show ownPk), ("bid", show b)]
  utxoMap <- utxoAt scrAddress
  let (oref, txOutTx@TxOutTx {txOutTxOut = txOut}) = head . Map.toList $ utxoMap
      Just oldBid = txOutTxToBid txOutTx
      tokenValue =
        let Value x = txOut ^. outValue
            r = AssocMap.delete Ada.adaSymbol x
         in Value r
  let lookups =
        Constraints.unspentOutputs utxoMap
          <> Constraints.scriptInstanceLookups scrInst
          <> Constraints.otherScript scr
      constraints =
        mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData $ PayBackInput ownBid)
          <> mustPayToPubKey (bBidder oldBid) (Ada.toValue $ bBid oldBid)
          <> mustPayToTheScript (PayBackDatum ownBid) (tokenValue <> Ada.toValue (bBid ownBid))
  logI'
    "Paying back"
    [ ("old bid", show oldBid),
      ("own bid", show ownBid),
      ("token value", show tokenValue)
    ]
  ledgerTx <- submitTxConstraintsWith lookups constraints
  logI "Waiting for tx confirmation"
  void . awaitTxConfirmed . txId $ ledgerTx

  -- Print UTxO
  logI "Printing for tx confirmation"
  printUTxODatums scrAddress
  pure ()

endpoints :: PayBackContract ()
endpoints = (start' `select` bid') >> endpoints
  where
    start' = endpoint @"start" >> start
    bid' = endpoint @"bid" >>= bid

-- | Tests
test :: IO ()
test = Emulator.runEmulatorTraceIO $ do
  h1 <- Emulator.activateContractWallet w1 endpoints
  h2 <- Emulator.activateContractWallet w2 endpoints
  h3 <- Emulator.activateContractWallet w3 endpoints
  -- Starting
  Extras.logInfo @String $ "Wallet 1 starts auction"
  Emulator.callEndpoint @"start" h1 ()
  -- Bidding
  void $ Emulator.waitUntilSlot 5
  Extras.logInfo @String $ "Wallet 2 bids"
  Emulator.callEndpoint @"bid" h2 400
  void $ Emulator.waitNSlots 1
  Extras.logInfo @String $ "Wallet 3 bids"
  Emulator.callEndpoint @"bid" h3 500
  -- -- Closing
  -- void $ waitUntilSlot (pEndTime auction)
  -- -- FIXME Close with any wallet
  -- callEndpoint @"close" h1 auction
  s <- Emulator.waitNSlots 1
  Extras.logInfo $ "Exit" ++ show s

w1, w2, w3, w4 :: Wallet.Wallet
w1 = Wallet.Wallet 1
w2 = Wallet.Wallet 2
w3 = Wallet.Wallet 3
w4 = Wallet.Wallet 4

walletPubKeyHash :: Wallet.Wallet -> PubKeyHash
walletPubKeyHash = pubKeyHash . Wallet.walletPubKey

-- | General helpers
txOutTxToBid :: TxOutTx -> Maybe Bid
txOutTxToBid o = txOutTxDatum o >>= datumToBid

datumToBid :: Datum -> Maybe Bid
datumToBid (Datum d) = do
  PayBackDatum b <- PlutusTx.fromData @PayBackDatum d
  pure b

printUTxODatums :: Ledger.Address -> PayBackContract ()
printUTxODatums scrAddr = do
  utxoMap <- utxoAt scrAddr
  logI'' "UTxO count" "count" $ show (Map.size utxoMap)
  let datums :: [(Value, PayBackDatum)] =
        utxoMap
          ^.. folded
            . Control.Lens.to (\o -> (txOutValue $ txOutTxOut o,) <$> txOutTxDatum o)
            . _Just
            . Control.Lens.to (\(v, Datum d) -> (v,) <$> PlutusTx.fromData @PayBackDatum d)
            . _Just
  PlutusTx.Prelude.mapM_ (logI'' "UTxO datums" "datums") $ fmap show datums

logI :: Haskell.String -> Contract w s e ()
logI = logInfo @Haskell.String

-- TODO (sometime): Replace value tuples with HList.
logI' :: Haskell.String -> [(Haskell.String, Haskell.String)] -> Contract w s e ()
logI' t m = logInfo @Haskell.String $ t <> printKeyValues m
  where
    printKeyValues [] = ""
    printKeyValues m' = ": " <> mconcat (fmap kvToString m')
    kvToString (k, v) = k <> "=" <> show v

logI'' :: Haskell.String -> Haskell.String -> Haskell.String -> Contract w s e ()
logI'' t k v = logI' t [(k, v)]

logUTxOSize :: Maybe Haskell.String -> UtxoMap -> Contract w s e ()
logUTxOSize title utxoMap =
  let t = fromMaybe "UTxO size of script" title
   in logI'' t "size" $ show (Map.size utxoMap)
