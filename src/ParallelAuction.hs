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
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module ParallelAuction where

import Control.Lens
import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (hash)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import Ledger.AddressMap (UtxoMap)
import Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Plutus.Contract
  ( AsContractError (_ContractError),
    BlockchainActions,
    Contract,
    ContractError (OtherError),
    Endpoint,
    awaitTxConfirmed,
    endpoint,
    logInfo,
    ownPubKey,
    select,
    submitTxConstraints,
    submitTxConstraintsWith,
    throwError,
    utxoAt,
    type (.\/),
  )
import qualified Plutus.Contracts.Currency as Currency
import qualified Plutus.V1.Ledger.Interval as Interval
import qualified Plutus.V1.Ledger.Value as Value
import qualified PlutusTx
import PlutusTx.AssocMap as AssocMap (delete, lookup, singleton)
import PlutusTx.Prelude
  ( AdditiveGroup ((-)),
    Applicative (pure),
    Bool (..),
    Eq ((==)),
    Functor (fmap),
    Int,
    Integer,
    Integral (mod),
    Maybe (..),
    Ord (compare, (<)),
    Ordering (EQ, GT),
    Show (show),
    fromIntegral,
    fromMaybe,
    id,
    length,
    mapM_,
    maximum,
    mconcat,
    replicate,
    trace,
    traceIfFalse,
    ($),
    (&&),
    (.),
    (/=),
    (<$>),
  )
import Prelude (Semigroup (..))
import qualified Prelude as Haskell

-- Idea:
--
--
-- # Question
-- - How does Datum look like?
-- - How does spin offs look like?
-- - How does closing Tx look like
--
--
-- # Validator
-- For
--
-- # Endpoints
-- - Create
-- - Bid
-- - Close
--   - Anybody, incentivized by highest bidder
--
-- # Initiator / Closer
-- - Forge tokens
--
--
-- # Bidder
-- - Choose one of the possible UTxOs for bidding
-- - Maybe something with hash and wrap around number of availabe UTxO
-- - Look at all at once and choose a max
-- - Bid by extending one of the UTxOs
--
-- # Close
-- - Tx must have all UTxO of initial auction
-- - Auction collects highest bids of all threads and computes highest bit
-- - Pay back non-highest bid
--
-- # Test
-- - Allow multiple bidders in one Slot / at the same time
-- - Bids on same UTxO lead to some invalid Tx
-- - But most should go through if enough open UTxO
--
--

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

-- FIXME Rename to ParallelAuctionState
data ParallelAuctionDatum
  = -- | State which holds asset
    Hold
  | -- | State for bidding threads
    Bidding {dHighestBid :: Bid}
  | -- | Auction was closed
    Finished
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''ParallelAuctionDatum

data ParallelAuctionInput
  = InputBid Bid
  | InputClose

PlutusTx.unstableMakeIsData ''ParallelAuctionInput

{-# INLINEABLE mustDistributeThreadTokensWithInitBid #-}
mustDistributeThreadTokensWithInitBid :: Bid -> [Value] -> TxConstraints i ParallelAuctionDatum
mustDistributeThreadTokensWithInitBid initialBid tokenValues =
  mconcat $ fmap (mustPayToTheScript $ Bidding initialBid) tokenValues

{-# INLINEABLE mustPayAssetFromOwnerToScript #-}
mustPayAssetFromOwnerToScript :: Value -> TxConstraints i ParallelAuctionDatum
mustPayAssetFromOwnerToScript = mustPayToTheScript Hold

-- | Untyped redeemer for 'InputClose' input.
{-# INLINEABLE closeRedeemer #-}
closeRedeemer :: Redeemer
closeRedeemer = Redeemer $ PlutusTx.toData InputClose

{-# INLINEABLE mustPayOwner #-}
mustPayOwner :: ParallelAuctionParams -> TxOutRef -> Bid -> TxConstraints i o
mustPayOwner params winningUtxoRef highestBid =
  mustSpendScriptOutput winningUtxoRef (Redeemer $ PlutusTx.toData InputClose)
    <> mustPayToPubKey (pOwner params) (Ada.toValue $ bBid highestBid)

{-# INLINEABLE mustTransferAsset #-}
mustTransferAsset :: ParallelAuctionParams -> TxOutRef -> Bid -> TxConstraints i o
mustTransferAsset params holdUtxoRef highestBid =
  mustSpendScriptOutput holdUtxoRef closeRedeemer
    <> mustPayToPubKey (bBidder highestBid) (pAsset params)

-- | Returns thread tokens back to script.
--   FIXME Better to burn them? How to burn these tokens? Using Currency from use-cases.
{-# INLINEABLE mustReturnThreadTokens #-}
mustReturnThreadTokens :: Value -> TxConstraints i ParallelAuctionDatum
mustReturnThreadTokens = mustPayToTheScript Finished

{-# INLINEABLE mustUseThreadTokenAndPayBid #-}
mustUseThreadTokenAndPayBid :: TxOutRef -> Value -> Bid -> TxConstraints i ParallelAuctionDatum
mustUseThreadTokenAndPayBid utxoBidRef threadToken bid =
  let inputBid = InputBid bid
      outputBid = Bidding bid
      payToScript = threadToken <> Ada.toValue (bBid bid)
   in mustSpendScriptOutput utxoBidRef (Redeemer $ PlutusTx.toData inputBid)
        <> mustPayToTheScript outputBid payToScript

{-# INLINEABLE mustPayBackBid #-}
mustPayBackBid :: Bid -> TxConstraints i ParallelAuctionDatum
mustPayBackBid oldBid = mustPayToPubKey (bBidder oldBid) (Ada.toValue $ bBid oldBid)

{-# INLINEABLE validAfterDeadline #-}
validAfterDeadline :: Slot -> TxConstraints i o
validAfterDeadline deadline = mustValidateIn (Interval.from deadline)

{-# INLINEABLE validBeforeDeadline #-}
validBeforeDeadline :: Slot -> TxConstraints i o
validBeforeDeadline deadline =
  -- FIXME @Lars: Using (Enum.pred deadline) leads to failing on-chain code. Intended?
  mustValidateIn (Interval.to $ deadline - 1)

-- | Version of 'checkScriptContext' with fixed types for this contract.
{-# INLINEABLE checkConstraints #-}
checkConstraints :: TxConstraints ParallelAuctionInput ParallelAuctionDatum -> ScriptContext -> Bool
checkConstraints = checkScriptContext @ParallelAuctionInput @ParallelAuctionDatum

-- TODO Checks:
-- - Ensure one input
-- - Ensure input contains correct token
-- - Only one output
-- - Output goes back to script
-- - Output contains token
{-# INLINEABLE validateNewBid #-}
validateNewBid :: ParallelAuctionParams -> ScriptContext -> Bid -> Bid -> Bool
validateNewBid params ctx@ScriptContext {scriptContextTxInfo = txInfo} curBid newBid =
  traceIfFalse
    "New bid is not higher"
    (checkNewBidIsHigher curBid newBid)
    && traceIfFalse
      "Auction is not open anymore"
      (checkConstraints (validBeforeDeadline $ pEndTime params) ctx)
    && traceIfFalse
      "Bid is not valid thread continuation"
      (checkIsBiddingThread ctx)

{-# INLINEABLE validateCloseBiddingThread #-}
validateCloseBiddingThread :: ParallelAuctionParams -> ScriptContext -> Bid -> Bool
validateCloseBiddingThread params ctx bid =
  traceIfFalse
    "No valid closing transaction"
    (validateIsClosingTx params ctx)
    && trace "Closing bidding" True

{-# INLINEABLE validateCloseAuction #-}
validateCloseAuction :: ParallelAuctionParams -> ScriptContext -> Bool
validateCloseAuction params ctx@ScriptContext {scriptContextTxInfo = txInfo} =
  traceIfFalse
    "No valid closing transaction"
    (validateIsClosingTx params ctx)
    && trace "Closing auction" True

{-# INLINEABLE validateIsClosingTx #-}
-- TODO Checks
-- - Closes all bidding threads
-- - Spends highest bid to owner
-- - Spends hold UTxO (only one)
-- - Spends asset to highest bidder
validateIsClosingTx :: ParallelAuctionParams -> ScriptContext -> Bool
validateIsClosingTx params ctx@ScriptContext {scriptContextTxInfo = txInfo} =
  True

{-# INLINEABLE mkValidator #-}
mkValidator ::
  ParallelAuctionParams ->
  ParallelAuctionDatum ->
  ParallelAuctionInput ->
  ScriptContext ->
  Bool
mkValidator params state input ctx =
  case (state, input) of
    -- Transition within a bidding thread
    (Bidding curBid, InputBid newBid) -> validateNewBid params ctx curBid newBid
    -- Transition from bidding thread to closed auction
    (Bidding curBid, InputClose) -> validateCloseBiddingThread params ctx curBid
    -- Transition from open auction to closed auction
    (Hold, InputClose) -> validateCloseAuction params ctx
    -- Not allowed transitions
    (Finished, _) ->
      trace "Invalid transition from state finished" False
    _ -> trace "Unknown transition" False

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
  | CheckError Text.Text
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

endpoints :: ParallelAuctionContract ()
endpoints = (start' `select` bid' `select` close') >> endpoints
  where
    start' = endpoint @"start" >>= start
    bid' = endpoint @"bid" >>= bid
    close' = endpoint @"close" >>= close

-- | Initiates the auction:
--   - Creates a new currency with a token amount equal to the thread count, thread tokens
--   - Each thread token gets it's own UTxO with an initial bid datum (state Bidding)
--   - The asset is transferred from the owner to the script
--   - The asset is hold in a separate UTxO with state Hold
start :: ParallelAuctionParams -> ParallelAuctionContract ()
start params = do
  -- General values
  ownPkHash <- pubKeyHash <$> ownPubKey
  let scrInst = inst params
  -- Create bidding threads (UTxOs)
  threadTokenValues <- createBiddingThreads ownPkHash (pThreadCount params)
  -- Create tx constraints
  let constraints =
        mustDistributeThreadTokensWithInitBid (Bid 0 ownPkHash) threadTokenValues
          <> mustPayAssetFromOwnerToScript (pAsset params)
  logI "Starting auction"
  ledgerTx <- submitTxConstraints scrInst constraints
  void . awaitTxConfirmed . txId $ ledgerTx
  -- Debug
  printUtxos' params

-- | Selects any of the existing thread UTxOs. For placing own bid by spending this UTxO.
selectUtxoIndex :: PubKeyHash -> Integer -> Int
selectUtxoIndex pkHash threadCount = hash pkHash `mod` fromIntegral threadCount

-- | Assuming thread token
extractThreadToken :: Value -> Value
extractThreadToken (Value.Value x) = Value.Value $ AssocMap.delete Ada.adaSymbol x

bid :: (ParallelAuctionParams, Integer) -> ParallelAuctionContract ()
bid (params, b) = do
  ownPkHash <- pubKeyHash <$> ownPubKey
  let scrInst = inst params
      scr = validator params
      scrAddr = scrAddress params
      ownBid = Bid (Ada.Lovelace b) ownPkHash
  logI' "Trying to place bid" [("pk hash", show ownPkHash), ("bid", show b)]
  utxoMap <- utxoAt scrAddr
  let threadUtxoMap = filterBiddingUTxOs utxoMap
      highestBidMaybe = highestBidInUTxOs threadUtxoMap
  case highestBidMaybe of
    Just highestBid | highestBid Haskell.< ownBid -> do
      let utxoIndex = selectUtxoIndex ownPkHash (pThreadCount params)
          -- FIXME Unsafe, but fine, assuming to have as many threads as UTxOs
          (utxoBidRef, txOut) = utxoIndex `Map.elemAt` threadUtxoMap
          threadToken = extractThreadToken $ txOutTxOut txOut ^. outValue
          -- Note: This is not necessarily the (computed) highest bid. It is the bid on the
          --   current thread.
          Just oldThreadBid = txOutTxToBid txOut
      logI'
        "Placing bid"
        [ ("UTxO index", show utxoIndex),
          ("highest bid", show highestBid)
        ]
      let lookups =
            Constraints.unspentOutputs utxoMap
              <> Constraints.scriptInstanceLookups scrInst
              <> Constraints.otherScript scr
          constraints =
            validBeforeDeadline (pEndTime params)
              <> mustPayBackBid oldThreadBid
              <> mustUseThreadTokenAndPayBid utxoBidRef threadToken ownBid
      -- Submit tx
      ledgerTx <- submitTxConstraintsWith lookups constraints
      void . awaitTxConfirmed . txId $ ledgerTx
    _ -> throwError . CheckError $ "Failed to find highest bid"

  -- Print UTxO
  logI "Printing for tx confirmation"
  printUtxos' params
  pure ()

close :: ParallelAuctionParams -> ParallelAuctionContract ()
close params = do
  let scrInst = inst params
      scr = validator params
      scrAddr = scrAddress params
  logI "Closing auction"
  utxoMap <- utxoAt scrAddr
  -- Debug
  printUtxos' params
  -- FIXME Ugly. Define how to handle filtering and selecting more nicely.
  -- Filter for utxo with bidding state
  let threadUtxoMap = filterBiddingUTxOs utxoMap
  case findMaxUTxORef threadUtxoMap of
    Just (winningUtxoRef, otherUtxoRefs) -> do
      let Just winningTxOut = Map.lookup winningUtxoRef threadUtxoMap
          Just highestBid = txOutTxToBid winningTxOut
          winningTxOutValue = txOutTxOut winningTxOut ^. outValue
          -- FIXME: May fail if multiple thread tokens / non-Ada tokens are found
          threadTokenValue = extractThreadToken winningTxOutValue
          threadTokensValue = Value.scale (pThreadCount params) threadTokenValue
          [holdUtxoRef] = Map.keys $ filterHoldUTxOs utxoMap
      logI'' "Highest bid" "" $ show winningTxOut
      logI'' "Thread token" "" $ show threadTokenValue
      logI'' "Thread token all" "" $ show threadTokensValue
      let lookups =
            Constraints.unspentOutputs utxoMap
              <> Constraints.scriptInstanceLookups scrInst
              <> Constraints.otherScript scr
          payBacks = mconcat $ payBackBid threadUtxoMap <$> otherUtxoRefs
          constraints =
            validAfterDeadline (pEndTime params)
              -- FIXME Ugly
              <> payBacks
              <> mustPayOwner params winningUtxoRef highestBid
              <> mustTransferAsset params holdUtxoRef highestBid
              <> mustReturnThreadTokens threadTokensValue
      -- Submit tx
      ledgerTx <- submitTxConstraintsWith lookups constraints
      logI "Waiting for tx confirmation"
      void . awaitTxConfirmed . txId $ ledgerTx
      -- Debug
      printUtxos' params
      pure ()
    Nothing -> do
      logI "Failed to find highest bid"
  pure ()
  where
    payBackBid utxoMap utxoRef =
      -- FIXME Unsafe
      let Just out = Map.lookup utxoRef utxoMap
          -- FIXME Unsafe
          Just (Bid b pkh) = txOutTxToBid out
       in mustSpendScriptOutput utxoRef closeRedeemer
            <> mustPayToPubKey pkh (Ada.toValue b)

-- Helper
createBiddingThreads ::
  PubKeyHash ->
  Integer ->
  ParallelAuctionContract [Value]
createBiddingThreads pkHash threadCount = do
  c :: _ <- Currency.forgeContract pkHash [("auction-threads", fromIntegral threadCount)]
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
    step Nothing (ref, txOut) = Just (ref, txOut, [])
    step (Just (aref, aTxOut, arefs)) (ref, txOut) =
      case Haskell.compare (txOutTxToBid txOut) (txOutTxToBid aTxOut) of
        GT -> Just (ref, txOut, aref : arefs)
        _ -> Just (aref, aTxOut, ref : arefs)

txOutTxToBid :: TxOutTx -> Maybe Bid
txOutTxToBid o = txOutTxDatum o >>= datumToBid

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
printUtxos' :: ParallelAuctionParams -> ParallelAuctionContract ()
printUtxos' = printUtxos . scrAddress

printUtxos :: Ledger.Address -> ParallelAuctionContract ()
printUtxos scrAddr = do
  utxoMap <- utxoAt scrAddr
  logI'' "UTxO count" "count" $ show (Map.size utxoMap)
  let datums :: [(Value, ParallelAuctionDatum)] =
        utxoMap
          ^.. folded
            . Control.Lens.to (\o -> (txOutValue $ txOutTxOut o,) <$> txOutTxDatum o)
            . _Just
            . Control.Lens.to (\(v, Datum d) -> (v,) <$> PlutusTx.fromData @ParallelAuctionDatum d)
            . _Just
  PlutusTx.Prelude.mapM_ (logI'' "UTxO datums" "datums") $ fmap show datums

logI :: Haskell.String -> Contract w s e ()
logI = logInfo @Haskell.String

-- TODO (sometime): Replace value tuples with HList.
logI' :: Haskell.String -> [(Haskell.String, Haskell.String)] -> Contract w s e ()
logI' t m = logInfo @Haskell.String $ t <> printKeyValues m
  where
    printKeyValues [] = ""
    printKeyValues m' = ": " <> List.intercalate ", " (fmap kvToString m')
    kvToString (k, v) = k <> "=" <> show v

logI'' :: Haskell.String -> Haskell.String -> Haskell.String -> Contract w s e ()
logI'' t k v = logI' t [(k, v)]

logUTxOSize :: Maybe Haskell.String -> UtxoMap -> Contract w s e ()
logUTxOSize title utxoMap =
  let t = fromMaybe "UTxO size of script" title
   in logI'' t "size" $ show (Map.size utxoMap)
