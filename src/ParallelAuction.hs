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
  = Ongoing
      { dHighestBid :: Bid
      -- FIXME: Required in here? Or just check that it is preserve.
      -- , dThreadToken :: Maybe Value.AssetClass
      }
  | Hold
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

-- Get utxos

-- TODO
-- - Make sure start can only be called once
-- - Make sure close only closes if done
endpoints :: Contract () ParallelAuctionSchema ParallelAuctionError ()
endpoints = (start' `select` bid' `select` close') >> endpoints
  where
    start' = endpoint @"start" >>= start
    bid' = endpoint @"bid" >>= bid
    close' = endpoint @"close" >>= close

start :: ParallelAuctionParams -> Contract w ParallelAuctionSchema ParallelAuctionError ()
start params = do
  ownPk <- ownPubKey
  let ownPkHash = pubKeyHash ownPk
      scrInst = inst params
  -- Forge tokens for auction threads
  c :: Currency.OneShotCurrency <-
    Currency.forgeContract (pubKeyHash ownPk) [("auction-threads", fromIntegral $ pThreadCount params)]
  let cV = Currency.forgedValue c
      -- Create one UTxO for every single value
      cVs = splitToSingleValues cV
      txConstrs = createConstraintsForValues ownPkHash cVs
        <> mustPayToTheScript Hold (pAsset params)
  logI "submit for tx confirmation"
  -- Submit tx
  ledgerTx <- submitTxConstraints scrInst txConstrs
  logI "wait for tx confirmation"
  void . awaitTxConfirmed . txId $ ledgerTx
  -- Verify current state
  printUTxODatums params
  where
    createConstraintForValue :: PubKeyHash -> Value -> TxConstraints ParallelAuctionInput ParallelAuctionDatum
    createConstraintForValue self = mustPayToTheScript (Ongoing $ Bid 0 self)
    createConstraintsForValues :: PubKeyHash -> [Value] -> TxConstraints ParallelAuctionInput ParallelAuctionDatum
    createConstraintsForValues self = mconcat . fmap (createConstraintForValue self)

splitToSingleValues :: Value -> [Value]
splitToSingleValues v = do
  (s, tn, amt) <- Value.flattenValue v
  -- TODO: Unsafe?
  replicate (fromIntegral amt) $ Value.singleton s tn 1

bid :: (ParallelAuctionParams, Integer) -> Contract w ParallelAuctionSchema ParallelAuctionError ()
bid (params, b) = do
  ownPk <- ownPubKey
  curSlot <- currentSlot
  let ownPkHash = pubKeyHash ownPk
      scrInst = inst params
      ownBid = Bid (Ada.Lovelace b) ownPkHash
      inputBid = InputBid ownBid
      -- Own bid should be highest
      -- - In best case: Overall
      -- - If parallel bids are done, it's should be at least the highest of the current thread
      -- - Or not valid at all
      outputBid = Ongoing ownBid
      validTo = Interval.to $ succ curSlot
  logI' "Placing bid" [("pk", show ownPk), ("bid", show b)]
  -- Verify current state
  -- TODO Filter out invalid/malicious utxos; how to know which token?
  utxoMap <- utxoAt (scrAddress params)
  logUTxOSize Nothing utxoMap
  printUTxODatums params
  let highestBid = highestBidInUTxOs utxoMap
  logI'' "Checking if bidding highest for all UTxOs" "highest bid" (show highestBid)
  -- Only bid if own bid is higher than highest
  -- Select an UTxOs by using public key hash and thread count
  let pkHash = pubKeyHash ownPk
      utxoIndex :: Int = hash pkHash `mod` fromIntegral (pThreadCount params)
      -- Ensure utxoMap has the same amount as `threadCount`
      -- FIXME: Replace with lens getter
      (utxoToBidRef, TxOutTx _ (TxOut _ threadToken _)) = utxoIndex `Map.elemAt` utxoMap

  logI'' "Choosing UTxO" "index" $ show utxoIndex
  let lookups =
        Constraints.unspentOutputs utxoMap
          <> Constraints.scriptInstanceLookups scrInst
      -- Built constraints on our own
      constrs = [MustValidateIn validTo]
      inputConstraints =
        [ InputConstraint
            { icRedeemer = inputBid,
              icTxOutRef = utxoToBidRef
            }
        ]
      -- FIXME: Datum should be the updated one from the existing tx
      outputConstraints =
        [ OutputConstraint
            { ocDatum = outputBid,
              ocValue = threadToken <> Ada.toValue (bBid ownBid)
            }
        ]
      txConstrs =
        TxConstraints
          { txConstraints = constrs,
            txOwnInputs = inputConstraints,
            txOwnOutputs = outputConstraints
          }
  -- Submit tx
  ledgerTx <- submitTxConstraintsWith lookups txConstrs
  logI "Waiting for tx confirmation"
  void . awaitTxConfirmed . txId $ ledgerTx

  -- Print UTxO
  logI "Printing for tx confirmation"
  printUTxODatums params
  -- utxoMap <- utxoAt (scrAddress params)
  -- let highestBid = highestBidInUTxOs utxoMap
  -- logI'' "Checking if bidding highest for all UTxOs" "highest bid" (show highestBid)

  -- TODO How to know which token?
  -- Use this UTxO to do the bidding
  pure ()


close :: ParallelAuctionParams -> Contract w ParallelAuctionSchema ParallelAuctionError ()
close params = do
  curSlot <- currentSlot
  let scrInst = inst params
      scr = validator params
      validFrom = Interval.from $ curSlot
  logI "Closing auction"
  -- FIXME: Only UTxOs with correct asset
  utxoMap <- utxoAt (scrAddress params)
  printUTxODatums params
  case findMaxUTxORef utxoMap of
    Just (winningUtxoRef, otherUtxoRefs) -> do
      logI "Paying back"
      let Just winningTxOut = Map.lookup winningUtxoRef utxoMap
          Just highestBid = txOutToBid winningTxOut
          winningTxOutValue = txOutTxOut winningTxOut ^. outValue
          -- FIXME: May fail if multiple thread tokens / non-Ada tokens are found
          [threadTokenSymbol] = List.filter (/= adaSymbol) $ Value.symbols winningTxOutValue
          Just winningTxOutThreadTokenValue = PlutusTxMap.lookup threadTokenSymbol $ Value.getValue winningTxOutValue
          winningTxOutThreadToken = negate . Value.Value $ singleton threadTokenSymbol winningTxOutThreadTokenValue
          winningTxOutThreadTokenAll = negate . Value.Value $ singleton threadTokenSymbol (fmap (\_ -> pThreadCount params) winningTxOutThreadTokenValue)
      logI'' "Highest bid" "" $ show winningTxOut
      logI'' "Thread token" "" $ show winningTxOutThreadToken
      let lookups =
            Constraints.unspentOutputs utxoMap
              <> Constraints.scriptInstanceLookups scrInst
              <> Constraints.otherScript scr
      let payBacks = mconcat $ payBackBid utxoMap <$> otherUtxoRefs
          payOwner =
              mustSpendScriptOutput winningUtxoRef closeRedeemer
              <> mustPayToPubKey (pOwner params) (Ada.toValue $ bBid highestBid)
          -- transferAsset = mempty
          -- FIXME Thread tokens are not destroyed but kept in script. How to burn/destroy?
          burnThreadTokens =
              mustPayToTheScript Finished winningTxOutThreadTokenAll
          txConstrs =
            payBacks
              <> payOwner
              <> burnThreadTokens
      -- Submit tx
      ledgerTx <- submitTxConstraintsWith lookups txConstrs
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
datumToBid (Datum d) = dHighestBid <$> PlutusTx.fromData @ParallelAuctionDatum d

highestBidInUTxOs :: UtxoMap -> Maybe Bid
highestBidInUTxOs utxoMap = do
  maximumByOf
    ( folded
        . Control.Lens.to txOutTxDatum
        . _Just
        . Control.Lens.to datumToHighestBid
        . _Just
    )
    Haskell.compare
    utxoMap
  where
    datumToHighestBid (Datum d) = dHighestBid <$> PlutusTx.fromData @ParallelAuctionDatum d

-- General Helper
safeMax :: Haskell.Ord a => [a] -> Maybe a
safeMax [] = Nothing
safeMax bs = Just $ maximum bs

-- Logging Helper
printUTxODatums :: ParallelAuctionParams -> Contract w ParallelAuctionSchema ParallelAuctionError ()
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
    printKeyValues m = ": " <> mconcat (fmap kvToString m)
    kvToString (k, v) = k <> "=" <> show v

logI'' :: Haskell.String -> Haskell.String -> Haskell.String -> Contract w s e ()
logI'' t k v = logI' t [(k, v)]

logUTxOSize :: Maybe Haskell.String -> UtxoMap -> Contract w s e ()
logUTxOSize title utxoMap =
  let t = fromMaybe "UTxO size of script" title
   in logI'' t "size" $ show (Map.size utxoMap)
