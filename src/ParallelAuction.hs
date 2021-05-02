{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}
module ParallelAuction where

import Control.Lens
import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (hash)
import qualified Data.Map as Map
import qualified Data.List as List
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
import qualified Plutus.V1.Ledger.Interval as Interval
import qualified Plutus.V1.Ledger.Scripts (unitDatum, unitRedeemer)
import qualified Plutus.V1.Ledger.Value as Value
import qualified PlutusTx
import PlutusTx.AssocMap as PlutusTxMap
import PlutusTx.Builtins
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import qualified PlutusTx.Prelude as PlutusTx
import Text.Printf (printf)
import Prelude (Semigroup (..))
import qualified Prelude as Haskell

data ParallelAuctionParams = ParallelAuctionParams
  { -- | Receiver of highest bid after auction was closed.
    pOwner :: PubKeyHash,
    -- | Asset to be bid on. Locked in auction during open period.
    --   transferred to highest bidder.
    pAsset :: Value,
    -- | End of bidding, only close can be called afterwards.
    pEndTime :: Slot,
    -- | Used parallelity for auction.
    pThreadCount :: Integer
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''ParallelAuctionParams

data Bid = Bid
  { bBid :: Ada,
    bBidder :: PubKeyHash
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Compare bids first by value, than by public key.
instance Haskell.Ord Bid where
  compare (Bid b1 pk1) (Bid b2 pk2) =
    case compare b1 b2 of
      EQ -> compare pk1 pk2
      o -> o

PlutusTx.unstableMakeIsData ''Bid

data ParallelAuctionDatum
  -- | State which holds asset
  = Hold
  -- | State for bidding threads
  | Bidding {dHighestBid :: Bid}
  -- | Auction was closed
  | Finished
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''ParallelAuctionDatum

data ParallelAuctionInput = InputBid Bid | InputClose

PlutusTx.unstableMakeIsData ''ParallelAuctionInput

{-# INLINEABLE mkValidator #-}
mkValidator :: ParallelAuctionParams -> ParallelAuctionDatum -> ParallelAuctionInput -> ScriptContext -> Bool
mkValidator params datum (InputBid bid) ctx@ScriptContext {scriptContextTxInfo = txInfo} =
  traceIfFalse
    "New bid is not higher"
    (checkNewBidIsHigher' datum bid)
    && traceIfFalse
      "Auction is not open anymore"
      (checkAuctionIsStillOpen params ctx)
    && traceIfFalse
      "Bid is not valid thread continuation"
      (checkIsBiddingThread ctx)
-- Ensure one input
-- Ensure input contains correct token
-- Only one output
-- Output goes back to script
-- Output contains token

mkValidator params _ InputClose _ = True
-- Check on transaction
-- - Must contain all (and only) inputs with thread token
-- - Must select the higheset bid
-- - Must pay back lower bids
-- - Must pay highest bid to offerer
-- - Must transfer asset to highest bidder
--
-- Collect all UTxO
-- Use only the UTxOs with a thread token
-- Must be spent, inputs to TX
-- Thread token must be burned in TX
-- Datums of all inputs must be used to compute highest bidder
-- Highest bidder must get asset token
-- Money must be transfered to original owner
-- Money of not-highset bidders must be returned

checkNewBidIsHigher :: Bid -> Bid -> Bool
checkNewBidIsHigher (Bid curBid _) (Bid newBid _) = curBid < newBid

checkNewBidIsHigher' :: ParallelAuctionDatum -> Bid -> Bool
checkNewBidIsHigher' datum = checkNewBidIsHigher (dHighestBid datum)

checkDeadlineNotReached :: Slot -> SlotRange -> Bool
checkDeadlineNotReached = after

checkAuctionIsStillOpen :: ParallelAuctionParams -> ScriptContext -> Bool
checkAuctionIsStillOpen params ScriptContext {scriptContextTxInfo = txInfo} =
  checkDeadlineNotReached (pEndTime params) (txInfoValidRange txInfo)

checkIsBiddingThread :: ScriptContext -> Bool
checkIsBiddingThread ctx =
  let os = getContinuingOutputs ctx
   in case os of
        [_] -> True
        _ -> length os == 1

data ParallelAuction

instance Scripts.ScriptType ParallelAuction where
  type DatumType ParallelAuction = ParallelAuctionDatum
  type RedeemerType ParallelAuction = ParallelAuctionInput

inst :: ParallelAuctionParams -> Scripts.ScriptInstance ParallelAuction
inst c =
  Scripts.validator @ParallelAuction
    ($$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode c)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @ParallelAuctionDatum @ParallelAuctionInput

validator :: ParallelAuctionParams -> Validator
validator = Scripts.validatorScript . inst

scrAddress :: ParallelAuctionParams -> Ledger.Address
scrAddress = scriptAddress . validator

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
    .\/ Endpoint "start" ParallelAuctionParams
    .\/ Endpoint "bid" (ParallelAuctionParams, Integer)
    .\/ Endpoint "close" ParallelAuctionParams

-- Simplify type for auction contract.
type ParallelAuctionContract a = Contract () ParallelAuctionSchema ParallelAuctionError a

-- Get utxos

-- TODO
-- - Make sure start can only be called once
-- - Make sure close only closes if done
endpoints :: ParallelAuctionContract ()
endpoints = (start' `select` bid' `select` close') >> endpoints
  where
    start' = endpoint @"start" >>= start
    bid' = endpoint @"bid" >>= bid
    close' = endpoint @"close" >>= close

start :: ParallelAuctionParams -> ParallelAuctionContract ()
start params = do
  -- General values
  ownPk <- ownPubKey
  let ownPkHash = pubKeyHash ownPk
      scrInst = inst params
  -- Create bidding threads
  ts <- createBiddingThreads ownPkHash (pThreadCount params)
  -- Create tx constraints
  let -- Constraint to pay one thread token to each thread with inital bidding state
      distributeThreadTokensToThreads = mconcat $
        fmap (mustPayToTheScript . Bidding $ Bid 0 ownPkHash) ts
      -- Constraint to pay offered asset to script, hold in script until auction closes
      payAssetFromOwnerToScript =
        mustPayToTheScript Hold (pAsset params)
  logI "Starting auction"
  ledgerTx <- submitTxConstraints scrInst $
      distributeThreadTokensToThreads
      <> payAssetFromOwnerToScript
  void . awaitTxConfirmed . txId $ ledgerTx
  logI "Started auction"
  -- Verify current state
  printUTxODatums params

bid :: (ParallelAuctionParams, Integer) -> ParallelAuctionContract ()
bid (params, b) = do
  ownPk <- ownPubKey
  curSlot <- currentSlot
  let ownPkHash = pubKeyHash ownPk
      scrInst = inst params
      ownBid = Bid (Ada.Lovelace b) ownPkHash
      inputBid = InputBid ownBid
      outputBid = Bidding ownBid
      validTo = Interval.to $ succ curSlot
  logI' "Trying to place bid" [("pk", show ownPk), ("bid", show b)]
  threadUtxoMap <- filterBiddingUTxOs <$> utxoAt (scrAddress params)
  let highestBid = highestBidInUTxOs threadUtxoMap
  logI'' "Checking if bidding highest for all UTxOs" "highest bid" (show highestBid)
  -- Only bid if own bid is higher than highest
  -- Select an UTxOs by using public key hash and thread count
  let utxoIndex :: Int = hash ownPkHash `mod` fromIntegral (pThreadCount params)
      -- FIXME Unsafe operations, ensure threadUtxoMap has the same amount as `threadCount`
      (utxoToBidRef, txOut) = utxoIndex `Map.elemAt` threadUtxoMap
      threadToken = txOutTxOut txOut ^. outValue

  logI'' "Choosing UTxO" "index" $ show utxoIndex
  let lookups =
        Constraints.unspentOutputs threadUtxoMap
          <> Constraints.scriptInstanceLookups scrInst
      -- Built constraints on our own
      allowOnlyBeforeDeadline =
          mustValidateIn validTo
      -- FIXME: Datum should be the updated one from the existing tx
      -- FIXME: Use constraint creation methods and combine instead of manyally creating
      inputConstraints =
        [ InputConstraint
            { icRedeemer = inputBid,
              icTxOutRef = utxoToBidRef
            }
        ]
      outputConstraints =
        [ OutputConstraint
            { ocDatum = outputBid,
              ocValue = threadToken <> Ada.toValue (bBid ownBid)
            }
        ]
      txConstrs =
        TxConstraints
          { txConstraints = [],
            txOwnInputs = inputConstraints,
            txOwnOutputs = outputConstraints
          }
  -- Submit tx
  ledgerTx <- submitTxConstraintsWith lookups $
      allowOnlyBeforeDeadline
      <> txConstrs
  logI "Waiting for tx confirmation"
  void . awaitTxConfirmed . txId $ ledgerTx

  -- Print UTxO
  logI "Printing for tx confirmation"
  printUTxODatums params
  pure ()


close :: ParallelAuctionParams -> ParallelAuctionContract ()
close params = do
  curSlot <- currentSlot
  let scrInst = inst params
      scr = validator params
      validFrom = Interval.from $ curSlot
  logI "Closing auction"
  utxoMap <- utxoAt (scrAddress params)
  -- Filter for utxo with bidding state
  let threadUtxoMap = filterBiddingUTxOs utxoMap
  printUTxODatums params
  case findMaxUTxORef threadUtxoMap of
    Just (winningUtxoRef, otherUtxoRefs) -> do
      logI "Paying back"
      let Just winningTxOut = Map.lookup winningUtxoRef threadUtxoMap
          Just highestBid = txOutToBid winningTxOut
          winningTxOutValue = txOutTxOut winningTxOut ^. outValue
          -- FIXME: May fail if multiple thread tokens / non-Ada tokens are found
          [threadTokenSymbol] = List.filter (/= adaSymbol) $ Value.symbols winningTxOutValue
          Just threadTokenValue = PlutusTxMap.lookup threadTokenSymbol $ Value.getValue winningTxOutValue
          threadTokenValueAll = Value.Value $ singleton threadTokenSymbol (fmap (\_ -> pThreadCount params) threadTokenValue)

          [holdUtxoRef] = Map.keys $ filterHoldUTxOs utxoMap
      logI'' "Highest bid" "" $ show winningTxOut
      logI'' "Thread token" "" $ show threadTokenSymbol
      let lookups =
            Constraints.unspentOutputs utxoMap
              <> Constraints.scriptInstanceLookups scrInst
              <> Constraints.otherScript scr
          allowOnlyAfterDeadline =
              mustValidateIn validFrom
          payBacks = mconcat $ payBackBid threadUtxoMap <$> otherUtxoRefs
          payOwner =
              mustSpendScriptOutput winningUtxoRef closeRedeemer
              <> mustPayToPubKey (pOwner params) (Ada.toValue $ bBid highestBid)
          transferAsset =
              mustSpendScriptOutput holdUtxoRef closeRedeemer
              <> mustPayToPubKey (bBidder highestBid) (pAsset params)
          -- FIXME Thread tokens are not destroyed but kept in script. How to burn/destroy?
          burnThreadTokens =
              mustPayToTheScript Finished threadTokenValueAll
      -- Submit tx
      ledgerTx <- submitTxConstraintsWith lookups $
            allowOnlyAfterDeadline
              <> payBacks
              <> payOwner
              <> transferAsset
              <> burnThreadTokens
      logI "Waiting for tx confirmation"
      void . awaitTxConfirmed . txId $ ledgerTx

      -- Print UTxO
      logI "Printing for tx confirmation"
      printUTxODatums params

      pure ()
    Nothing -> do
      logI "Failed to find highest bid"
  pure ()
  where
    closeRedeemer = Redeemer $ PlutusTx.toData InputClose
    payBackBid utxoMap utxoRef =
        -- FIXME Unsafe
        let Just out = Map.lookup utxoRef utxoMap
            Just (Bid b pkh) = txOutToBid out
         in mustSpendScriptOutput utxoRef closeRedeemer
              <> mustPayToPubKey pkh (Ada.toValue b)

-- Helper
createBiddingThreads ::
  PubKeyHash ->
  Integer ->
  ParallelAuctionContract [Value]
createBiddingThreads pkHash threadCount = do
  c <- Currency.forgeContract pkHash [("auction-threads", fromIntegral threadCount)]
  pure . toSingleValues . Currency.forgedValue $ c

toSingleValues :: Value -> [Value]
toSingleValues v = do
  (s, tn, amt) <- Value.flattenValue v
  replicate (fromIntegral amt) $ Value.singleton s tn 1

findMaxUTxORef :: UtxoMap -> Maybe (TxOutRef, [TxOutRef])
findMaxUTxORef utxoMap = do
  let bids :: Maybe (TxOutRef, TxOutTx, [TxOutRef]) = Haskell.foldl step Nothing (Map.toList utxoMap)
   in (\(a, _, b) -> (a, b)) <$> bids
  where
    step Nothing (ref, out) = Just (ref, out, [])
    step (Just (aref, aout, arefs)) (ref, out) =
        case Haskell.compare (txOutToBid out) (txOutToBid aout) of
          GT -> Just (ref, out, aref : arefs)
          _ -> Just (aref, aout, ref : arefs)

txOutToBid :: TxOutTx -> Maybe Bid
txOutToBid o = txOutTxDatum o >>= datumToBid

datumToBid :: Datum -> Maybe Bid
datumToBid (Datum d) =
  PlutusTx.fromData @ParallelAuctionDatum d >>= selectBidInBidding

datumToHold :: Datum -> Maybe Bool
datumToHold (Datum d) =
  selectHoldInBidding <$> PlutusTx.fromData @ParallelAuctionDatum d

selectBidInBidding :: ParallelAuctionDatum -> Maybe Bid
selectBidInBidding (Bidding b) = Just b
selectBidInBidding _ = Nothing

selectHoldInBidding :: ParallelAuctionDatum -> Bool
selectHoldInBidding Hold = True
selectHoldInBidding _ = False

highestBidInUTxOs :: UtxoMap -> Maybe Bid
highestBidInUTxOs utxoMap = do
  maximumByOf
    ( folded
        . Control.Lens.to txOutTxDatum
        . _Just
        . Control.Lens.to datumToBid
        . _Just
    )
    Haskell.compare
    utxoMap

filterBiddingUTxOs :: UtxoMap -> UtxoMap
filterBiddingUTxOs utxoMap =
  Map.fromList $
    Map.toList utxoMap
      ^.. folded
        . filteredBy
          ( _2
              . Control.Lens.to txOutTxDatum
              . _Just
              . Control.Lens.to datumToBid
              . _Just
          )

filterHoldUTxOs :: UtxoMap -> UtxoMap
filterHoldUTxOs utxoMap =
  Map.fromList $
    Map.toList utxoMap
      ^.. folded
        . filteredBy
          ( _2
              . Control.Lens.to txOutTxDatum
              . _Just
              . Control.Lens.to datumToHold
              . _Just
              . filtered id
          )

-- General Helper
safeMax :: Haskell.Ord a => [a] -> Maybe a
safeMax [] = Nothing
safeMax bs = Just $ maximum bs

-- Logging Helper
printUTxODatums :: ParallelAuctionParams -> ParallelAuctionContract ()
printUTxODatums params = do
  utxoMap <- utxoAt $ scrAddress params
  logI'' "UTxO count" "count" $ show (Map.size utxoMap)
  let datums :: [ParallelAuctionDatum] =
        utxoMap
          ^.. folded
            . Control.Lens.to txOutTxDatum
            . _Just
            . Control.Lens.to (\(Datum d) -> PlutusTx.fromData @ParallelAuctionDatum d)
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
