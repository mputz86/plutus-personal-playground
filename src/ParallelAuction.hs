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
    ContractError,
    Endpoint,
    awaitTxConfirmed,
    endpoint,
    logInfo,
    ownPubKey,
    select,
    submitTxConstraints,
    submitTxConstraintsWith,
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
mustUseThreadTokenAndPayBid :: TxOutRef -> Value -> Bid -> Bid -> TxConstraints i ParallelAuctionDatum
mustUseThreadTokenAndPayBid utxoBidRef threadToken bid oldBid =
  let inputBid = InputBid bid
      outputBid = Bidding bid
      payToScript = threadToken <> Ada.toValue (bBid bid)
   in mustSpendScriptOutput utxoBidRef (Redeemer $ PlutusTx.toData inputBid)
        <> mustPayToPubKey (bBidder oldBid) (Ada.toValue $ bBid oldBid)
        <> mustPayToTheScript outputBid payToScript

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
-- - Spends hold UTxO
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
  -- Debug: Print UTxOs
  printUtxos' params

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
  let highestBid = highestBidInUTxOs threadUtxoMap
  logI'' "Checking if bidding highest for all UTxOs" "highest bid" (show highestBid)
  -- TODO Add check against highest bid of all currently known UTxOs
  -- Select an UTxOs by using public key hash and thread count
  let utxoIndex :: Int = hash ownPkHash `mod` fromIntegral (pThreadCount params)
      -- FIXME Unsafe operations, ensure threadUtxoMap has the same amount as `threadCount`
      (utxoBidRef, txOut) = utxoIndex `Map.elemAt` threadUtxoMap
      threadToken =
        let Value.Value x = txOutTxOut txOut ^. outValue
            r = AssocMap.delete Ada.adaSymbol x
         in Value.Value r
      Just loosingBid = txOutToBid txOut

  logI'' "Choosing UTxO" "index" $ show utxoIndex
  let lookups =
        Constraints.unspentOutputs utxoMap
          <> Constraints.scriptInstanceLookups scrInst
          <> Constraints.otherScript scr
      constraints =
        validBeforeDeadline (pEndTime params)
          <> mustUseThreadTokenAndPayBid utxoBidRef threadToken ownBid loosingBid
  logI'
    "Paying back"
    [ ("bid", show loosingBid),
      ("own bid", show ownBid),
      ("thread token", show threadToken),
      ("unbalanced tx", show $ mkTx lookups constraints)
    ]
  ledgerTx <-
    submitTxConstraintsWith lookups constraints
  logI "Waiting for tx confirmation"
  void . awaitTxConfirmed . txId $ ledgerTx

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
  -- Filter for utxo with bidding state
  let threadUtxoMap = filterBiddingUTxOs utxoMap
  printUtxos' params
  case findMaxUTxORef threadUtxoMap of
    Just (winningUtxoRef, otherUtxoRefs) -> do
      logI "Paying back"
      let Just winningTxOut = Map.lookup winningUtxoRef threadUtxoMap
          Just highestBid = txOutToBid winningTxOut
          winningTxOutValue = txOutTxOut winningTxOut ^. outValue
          -- FIXME: May fail if multiple thread tokens / non-Ada tokens are found
          [threadTokenSymbol] = List.filter (/= adaSymbol) $ Value.symbols winningTxOutValue
          Just threadTokenValue = AssocMap.lookup threadTokenSymbol $ Value.getValue winningTxOutValue
          threadTokenValueAll = Value.Value $ singleton threadTokenSymbol (fmap (\_ -> pThreadCount params) threadTokenValue)

          [holdUtxoRef] = Map.keys $ filterHoldUTxOs utxoMap
      logI'' "Highest bid" "" $ show winningTxOut
      logI'' "Thread token" "" $ show threadTokenSymbol
      let lookups =
            Constraints.unspentOutputs utxoMap
              <> Constraints.scriptInstanceLookups scrInst
              <> Constraints.otherScript scr
          payBacks = mconcat $ payBackBid threadUtxoMap <$> otherUtxoRefs
          constraints =
            validAfterDeadline (pEndTime params)
              <> payBacks
              <> mustPayOwner params winningUtxoRef highestBid
              <> mustTransferAsset params holdUtxoRef highestBid
              <> mustReturnThreadTokens threadTokenValueAll
      -- Submit tx
      ledgerTx <- submitTxConstraintsWith lookups constraints
      logI "Waiting for tx confirmation"
      void . awaitTxConfirmed . txId $ ledgerTx

      -- Print UTxO
      logI "Printing for tx confirmation"
      printUtxos' params

      pure ()
    Nothing -> do
      logI "Failed to find highest bid"
  pure ()
  where
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
    printKeyValues m' = ": " <> mconcat (fmap kvToString m')
    kvToString (k, v) = k <> "=" <> show v

logI'' :: Haskell.String -> Haskell.String -> Haskell.String -> Contract w s e ()
logI'' t k v = logI' t [(k, v)]

logUTxOSize :: Maybe Haskell.String -> UtxoMap -> Contract w s e ()
logUTxOSize title utxoMap =
  let t = fromMaybe "UTxO size of script" title
   in logI'' t "size" $ show (Map.size utxoMap)
