{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
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

import Control.Lens hiding ((.=))
import qualified Data.Set as Set
import Data.Aeson.Encode.Pretty (encodePretty)
import Text.Pretty.Simple (pPrint, pString, pShow)
import Control.Monad hiding (fmap)
import Data.Aeson as Aeson (FromJSON, ToJSON, (.=))
import qualified Data.Aeson as Aeson (object, Value(..))
import qualified Data.Aeson.Types as Aeson
import Data.Hashable (hash)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import Ledger.AddressMap (UtxoMap)
import Ledger.Constraints as Constraints
  ( TxConstraints,
    checkScriptContext,
    mustPayToPubKey,
    mustPayToTheScript,
    mustSpendScriptOutput,
    mustIncludeDatum,
    mustValidateIn,
    otherScript,
    scriptInstanceLookups,
    unspentOutputs,
  )
import qualified Ledger.Typed.Scripts as Scripts
import Plutus.Contract
  ( AsContractError (_ContractError),
    BlockchainActions,
    Contract,
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
import PlutusTx.AssocMap as AssocMap
import qualified PlutusTx.Builtins
import PlutusTx.Prelude
  ( AdditiveGroup ((-)),
    AdditiveSemigroup ((+)),
    Applicative (pure),
    Bool (..),
    Eq ((==)),
    Functor (fmap),
    Int,
    Integer,
    Integral (mod),
    Maybe (..),
    Monoid (mempty),
    Ord (compare, (<), (<=)),
    Ordering (EQ, GT),
    Show (show),
    const,
    fromIntegral,
    fromMaybe,
    id,
    isJust,
    mapM_,
    maximum,
    mconcat,
    not,
    replicate,
    snd,
    trace,
    traceIfFalse,
    ($),
    (&&),
    (.),
    (<$>),
  )
import Prelude (Semigroup (..))
import qualified Prelude as Haskell
import Plutus.Contract.Types (ContractError)
import LoggingUtil

-- Parallel Auction, Idea
--
-- Start
-- - Start the auction by creating multiple, parallel spendable, UTxOs for bidding
--   - These UTxOs are called (bidding) threads
--   - The count of parallel threads is called thread count
-- - Create a OneShotCurrency with as many coins as thread count
--   - These are called thread tokens
-- - Each of the bidding threads holds 1 thread token
-- - Create another UTxO which holds the asset of the auction
--
-- During bidding phase
-- - A bidder selects all currently know bidding threads
-- - Checks if his own bid is higher than the highest
-- - The bidder computes, based on his public key hash, a "random" UTxO to place his bid
--   - Maybe add current slot number, so that same bidder does not bid on same thread all the time
-- - Note: Selected bidding thread does not have to be the one with the highest bid
-- - The bidder gets the current bid on this thread, pays back the money to the original bidder
-- - The bidder places his own bid by spending the selected bidding thread UTxO
-- - The newly created UTxO contains the bidder's bid and the "forwarded" thread token
-- - This allows for parallel bids of at most the thread count
--   - But only if the computed index for the same slot do not collide
--
-- Closing
-- - Anybody can close (pays fees)
-- - From all bidding threads, select the one with the highest bid
-- - Pay back all other bidders
-- - Transfer the asset from the hold UTxO to the highest bidder
-- - Transfer the amount of the highest bid to the owner
--
-- Testing
-- - Make sure that thread tokens prevent that 'anybodys' UTxO sent to script address disturbs auction
--   - "Ensure continuation"
-- - Ensure that hold UTxO is not anybody's UTxO with same state, by accident (i.e. it must have been created by script)
-- - Ensure previous bidders get back money if new bid is placed
-- - Ensure parallel bidding is working
-- - Ensure selecting highest bid on close is working
-- - ...
--
-- Idea / To Clarify:
-- - Required to "know" the currency symbol of the newly created currency for the tokens within the contract requests (i.e. bidding)?
--   - To ensure the right token is consumed and spent?
--   - Probably not as long as we check that the UTxO comes from the script
--   - Does not checking in Contract allow attackers to "infiltrate" invalid UTxOs in the list of honest bidders? I.e. their contracts fail due to a mismatching bidding thread UTxO count)
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

data ParallelAuctionState
  = -- | State which holds asset
    Hold
  | -- | State for bidding threads
    --   TODO Should be sufficient to just store pub key hash of bidder since value is at UTxO
    Bidding {dHighestBid :: Bid}
  | -- | Auction was closed
    Finished
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''ParallelAuctionState

data ParallelAuctionInput
  = InputBid Bid
  | InputClose

PlutusTx.unstableMakeIsData ''ParallelAuctionInput

-- | Turns a zero value into a Nothing.
{-# INLINEABLE getNonZeroValue #-}
getNonZeroValue :: Value -> Maybe Value
getNonZeroValue v = if Value.isZero v then Nothing else Just v

-- | Split value into native and other currencies
{-# INLINEABLE splitNativeAndOthers #-}
splitNativeAndOthers :: Value -> (Maybe Value, Maybe Value)
splitNativeAndOthers (Value.Value x) =
  let native =
        Value.Value . AssocMap.singleton Ada.adaSymbol
          <$> AssocMap.lookup Ada.adaSymbol x
      tokens = getNonZeroValue . Value.Value $ AssocMap.delete Ada.adaSymbol x
   in (native, tokens)

{-# INLINEABLE splitNativeAndThreadToken #-}
splitNativeAndThreadToken :: Value -> (Maybe Value, Maybe Value)
splitNativeAndThreadToken v =
  let (native, others) = splitNativeAndOthers v
      token = do
        os <- others
        case Value.flattenValue os of
          [(cs, tn, a)] | a == 1 -> Just $ Value.singleton cs tn 1
          -- Fail because of: No token, wrong amount of single token or too many tokens
          _ -> Nothing
   in (native, token)

{-# INLINEABLE extractThreadToken #-}
extractThreadToken :: Value -> Maybe Value
extractThreadToken = snd . splitNativeAndThreadToken

-- | Untyped redeemer for 'InputClose' input.
{-# INLINEABLE closeRedeemer #-}
closeRedeemer :: Redeemer
closeRedeemer = Redeemer $ PlutusTx.toData InputClose

{-# INLINEABLE checkNewBidIsHigher #-}
checkNewBidIsHigher :: Bid -> Bid -> Bool
checkNewBidIsHigher (Bid curBid _) (Bid newBid _) = curBid < newBid

{-# INLINEABLE mustDistributeThreadTokensWithInitBid #-}
mustDistributeThreadTokensWithInitBid :: Bid -> [Value] -> TxConstraints i ParallelAuctionState
mustDistributeThreadTokensWithInitBid initialBid tokenValues =
  mconcat $ fmap (mustPayToTheScript $ Bidding initialBid) tokenValues

{-# INLINEABLE mustPayAssetFromOwnerToScript #-}
mustPayAssetFromOwnerToScript :: Value -> TxConstraints i ParallelAuctionState
mustPayAssetFromOwnerToScript = mustPayToTheScript Hold

{-# INLINEABLE mustPayOwner #-}
mustPayOwner :: ParallelAuctionParams -> TxOutRef -> Bid -> TxConstraints i o
mustPayOwner params winningUtxoRef highestBid =
  mustSpendScriptOutput winningUtxoRef closeRedeemer
    <> mustPayToPubKey (pOwner params) (Ada.toValue $ bBid highestBid)

{-# INLINEABLE mustTransferAsset #-}
mustTransferAsset :: ParallelAuctionParams -> TxOutRef -> Bid -> TxConstraints i o
mustTransferAsset params holdUtxoRef highestBid =
  mustSpendScriptOutput holdUtxoRef closeRedeemer
    <> mustPayToPubKey (bBidder highestBid) (pAsset params)

-- | Returns thread tokens back to script.
--   FIXME Better to burn them? How to burn these tokens? Using Currency from use-cases.
{-# INLINEABLE mustReturnThreadTokens #-}
mustReturnThreadTokens :: Value -> TxConstraints i ParallelAuctionState
mustReturnThreadTokens = mustPayToTheScript Finished

{-# INLINEABLE mustUseThreadTokenAndPayBid #-}
mustUseThreadTokenAndPayBid :: TxOutRef -> Value -> Bid -> TxConstraints i ParallelAuctionState
mustUseThreadTokenAndPayBid utxoBidRef threadToken bid =
  let inputBid = InputBid bid
      outputBid = Bidding bid
      -- FIXME Combining values or paying twice to script is identical, i.e. only ONE UTxO is created
      --   Expected?
      payToScript = threadToken <> Ada.toValue (bBid bid)
   in mustSpendScriptOutput utxoBidRef (Redeemer $ PlutusTx.toData inputBid)
        <> mustPayToTheScript outputBid payToScript

{-# INLINEABLE mustPayBackBid #-}
mustPayBackBid :: Bid -> TxConstraints i ParallelAuctionState
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
checkConstraints :: TxConstraints ParallelAuctionInput ParallelAuctionState -> ScriptContext -> Bool
checkConstraints = checkScriptContext @ParallelAuctionInput @ParallelAuctionState

{-# INLINEABLE traceWithIfNothing' #-}
traceWithIfNothing' :: PlutusTx.Builtins.String -> Maybe a -> Maybe a
traceWithIfNothing' s = \case
  Nothing -> trace s Nothing
  Just v -> pure v

{-# INLINEABLE traceWithIfFalse' #-}
traceWithIfFalse' :: PlutusTx.Builtins.String -> Bool -> Maybe ()
traceWithIfFalse' s v = if not v then trace s Nothing else pure ()

{-# INLINEABLE validateNewBid #-}
validateNewBid :: ParallelAuctionParams -> ScriptContext -> Bid -> Bid -> Bool
validateNewBid params ctx@ScriptContext {scriptContextPurpose = Spending txOutRef} curBid newBid = isJust $ do
  -- Ensure new bid is higher than current
  traceWithIfFalse'
    "New bid is not higher"
    (checkNewBidIsHigher curBid newBid)
  -- Ensure there is one output which is continued
  TxOut {txOutValue} <- traceWithIfNothing'
    "More than one continuing output"
    $ case getContinuingOutputs ctx of
      [t] -> Just t
      _ -> Nothing
  -- Ensure this output contains the bidding thread token
  threadToken <-
    traceWithIfNothing' "Failed to extract thread token" $ extractThreadToken txOutValue
  -- Check tx constraints
  -- Check if bid is happending before deadline
  traceWithIfFalse'
    "Auction is not open anymore"
    (checkConstraints (validBeforeDeadline $ pEndTime params) ctx)
  -- Check if old bid is payed back
  traceWithIfFalse'
    "New bid does not pay back old bidder"
    (checkConstraints (mustPayBackBid curBid) ctx)
  -- Check if new bid is payed to script
  -- FIXME Constraints checks a little bit too much. In order to save some computation instructions
  --   (and therefore costs), split constraint is smaller ones and reuse.
  traceWithIfFalse'
    "New bid does not pay back old bidder"
    (checkConstraints (mustUseThreadTokenAndPayBid txOutRef threadToken newBid) ctx)
validateNewBid _ _ _ _ =
  traceIfFalse "Wrong script purpose for new bid" False

{-# INLINEABLE validateCloseBiddingThread #-}
validateCloseBiddingThread :: ParallelAuctionParams -> ScriptContext -> Bool
validateCloseBiddingThread params ctx =
  -- Check if bidding thread is consued by (valid) auction closing tx
  traceIfFalse
    "No valid closing transaction"
    (validateIsClosingTx params ctx)
    && trace "Closing bidding" True

{-# INLINEABLE validateCloseAuction #-}
validateCloseAuction :: ParallelAuctionParams -> ScriptContext -> Bool
validateCloseAuction params ctx =
  -- Check if hold UTxO is consued by (valid) auction closing tx
  traceIfFalse
    "No valid closing transaction"
    (validateIsClosingTx params ctx)
    && trace "Closing auction" True

-- | Counts how many inputs fulfill the predicate.
{-# INLINEABLE countState #-}
countState :: (ParallelAuctionState -> Bool) -> TxInfo -> [TxInInfo] -> Integer
countState stateCheckF txInfo = go
  where
    go [] = 0
    go (TxInInfo {txInInfoResolved = txOut} : is) =
        (if hasState txOut then 1 else 0) + go is
    hasState txOut = fromMaybe False $ do
      d <- getDatum txOut
      pure $ stateCheckF d
    getDatum txOut = do
      h <- txOutDatumHash txOut
      Datum d <- findDatum h txInfo
      PlutusTx.fromData @ParallelAuctionState d

countHold = countState $ \case
  Hold -> True
  _ -> False

countBidding = countState $ \case
  Bidding _ -> True
  _ -> False

-- | Used to validate if bidding threads and hold UTxO is consumed by auction closing tx.
--   TODO A lot to do here
{-# INLINEABLE validateIsClosingTx #-}
validateIsClosingTx :: ParallelAuctionParams -> ScriptContext -> Bool
validateIsClosingTx params ScriptContext{scriptContextTxInfo=txInfo} = isJust $ do
  let is = txInfoInputs txInfo
      holdCount = countHold txInfo (txInfoInputs txInfo)
      biddingCount = countBidding txInfo (txInfoInputs txInfo)
  traceWithIfFalse'
     "Consumes not exactly 1 hold state"
     $ holdCount == 1
  traceWithIfFalse'
     "Consumes not exactly as many bidding threads as threads"
     $ biddingCount == pThreadCount params
  -- TODO Check: Spends all bidding threads (goes to script; burning would be better)
  -- TODO Check: Spends hold UTxO (only one; goes to script)
  -- TODO Check: Spends asset to highest bidder
  -- TODO Check: Spends highest bid to owner
  -- TODO Check: Pays back any non-winning bids to owners
  pure ()

{-# INLINEABLE mkValidator #-}
mkValidator ::
  ParallelAuctionParams ->
  ParallelAuctionState ->
  ParallelAuctionInput ->
  ScriptContext ->
  Bool
mkValidator params state input ctx =
  case (state, input) of
    -- Transition within a bidding thread
    (Bidding curBid, InputBid newBid) -> validateNewBid params ctx curBid newBid
    -- Transition from bidding thread to closed auction
    (Bidding _, InputClose) -> validateCloseBiddingThread params ctx
    -- Transition from open auction to closed auction
    (Hold, InputClose) -> validateCloseAuction params ctx
    -- Not allowed transitions
    (Finished, _) ->
      trace "Invalid transition from state finished" False
    _ -> trace "Unknown transition" False

data ParallelAuction

instance Scripts.ScriptType ParallelAuction where
  type DatumType ParallelAuction = ParallelAuctionState
  type RedeemerType ParallelAuction = ParallelAuctionInput

inst :: ParallelAuctionParams -> Scripts.ScriptInstance ParallelAuction
inst c =
  Scripts.validator @ParallelAuction
    ($$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode c)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @ParallelAuctionState @ParallelAuctionInput

validator :: ParallelAuctionParams -> Validator
validator = Scripts.validatorScript . inst

scrAddress :: ParallelAuctionParams -> Ledger.Address
scrAddress = scriptAddress . validator

-- | Contract Errors
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

-- | Contract endpoints and type
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

-- | Starts an auction
start :: ParallelAuctionParams -> ParallelAuctionContract ()
start params = do
  ownPkHash <- pubKeyHash <$> ownPubKey
  -- Create tokens for bidding threads
  threadTokenValues <- createBiddingThreads ownPkHash (pThreadCount params)
  -- Create tx: One UTxO per bidding thread and one UTxO with asset
  let scrInst = inst params
      constraints =
        mustDistributeThreadTokensWithInitBid (Bid 0 ownPkHash) threadTokenValues
          <> mustPayAssetFromOwnerToScript (pAsset params)
  logI "Starting auction"
  ledgerTx <- submitTxConstraints scrInst constraints
  -- logI'' "Waiting for tx confirmation" "ledger tx" ledgerTx
  void . awaitTxConfirmed . txId $ ledgerTx
  -- Debug
  printUtxos' params

-- | Places bid
bid :: (ParallelAuctionParams, Integer) -> ParallelAuctionContract ()
bid (params, bidAmount) = do
  ownPkHash <- pubKeyHash <$> ownPubKey
  let scrInst = inst params
      scr = validator params
      scrAddr = scrAddress params
      ownBid = Bid (Ada.Lovelace bidAmount) ownPkHash
  logI'
    "Trying to place bid"
    [ "pk hash" .= ownPkHash,
      "bid" .= bidAmount
    ]
  utxoMap <- utxoAt scrAddr
  let threadUtxoMap = filterBiddingThreadUTxOs utxoMap
  -- Check if thread UTxOs and expected thread count match
  failWithIfFalse
    ( CheckError $
        toLogT
          "Failed since wrong thread UTxO count"
          [ "thread count" .= pThreadCount params,
            "utxo count" .= Map.size threadUtxoMap
          ]
    )
    (hasCorrectUtxoCount (pThreadCount params) (Map.size threadUtxoMap))
  -- Get highest bid
  highestBid <- case highestBidInUTxOs threadUtxoMap of
    Nothing ->
      throwError . CheckError $ "Failed to find highest bid"
    Just b
      | b Haskell.>= ownBid ->
        throwError . CheckError $ "Failed since another bid is higher than own bid"
    Just b -> pure b
  -- Select any bidding UTxO thread to place own bid
  let utxoIndex = selectUtxoIndex ownPkHash (pThreadCount params)
      -- Note: Unsafe, but checked that threadUtxoMap is equal to thread count
      (utxoBidRef, txOut) = utxoIndex `Map.elemAt` threadUtxoMap
      (_, threadTokenMaybe) = splitNativeAndThreadToken $ txOutTxOut txOut ^. outValue
      -- Note: This is the bid on the current thread and therefore not necessarily the highest bid (of all currently known UTxOs).
      oldThreadBidMaybe = txOutTxToBid txOut
  threadToken <-
    failWithIfNothing
      (CheckError "No thread token found in bidding thread")
      threadTokenMaybe
  oldThreadBid <-
    failWithIfNothing
      (CheckError "No old bid found in bidding thread")
      oldThreadBidMaybe
  logI'
    "Placing bid"
    [ "bidding UTxO thread index" .= utxoIndex,
      "highest bid" .= highestBid,
      "thread token" .= threadToken,
      "old bid" .= oldThreadBid
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
  -- logI'' "Waiting for tx confirmation" "ledger tx" ledgerTx
  void . awaitTxConfirmed . txId $ ledgerTx

  -- Print UTxO
  logI "Printing for tx confirmation"
  printUtxos' params
  pure ()

-- | Closes auction
close :: ParallelAuctionParams -> ParallelAuctionContract ()
close params = do
  let scrInst = inst params
      scr = validator params
      scrAddr = scrAddress params
  logI "Closing auction"
  utxoMap <- utxoAt scrAddr
  -- Debug
  printUtxos' params
  -- Filter for UTxOs with bidding state
  let threadUtxoMap = filterBiddingThreadUTxOs utxoMap
  -- Check if thread UTxOs and expected thread count match
  failWithIfFalse
    ( CheckError $
        toLogT
          "Failed since wrong thread UTxO count"
          [ "thread count" .= pThreadCount params,
            "utxo count" .= Map.size threadUtxoMap
          ]
    )
    (hasCorrectUtxoCount (pThreadCount params) (Map.size threadUtxoMap))
  -- Select winning UTxO
  (winningUtxoRef, otherUtxoRefs) <-
    failWithIfNothing
      (CheckError "Failed to find highest bin in all bidding UTxOs")
      (findMaxUTxORef threadUtxoMap)
  (winningTxOut, highestBid) <- failWithIfNothing
    (CheckError "Failed to receive winning UTxO from UTxO ref")
    $ do
      woref <- Map.lookup winningUtxoRef threadUtxoMap
      wbid <- txOutTxToBid woref
      pure (woref, wbid)
  let winningTxOutValue = txOutTxOut winningTxOut ^. outValue
      threadTokenMaybe = extractThreadToken winningTxOutValue
  threadToken <-
    failWithIfNothing
      (CheckError "No thread token found in winner bidding thread")
      threadTokenMaybe
  let threadTokensValue = Value.scale (pThreadCount params) threadToken
      holdUtxoRefs = Map.keys $ filterHoldUTxOs utxoMap
  holdUtxoRef <-
    case holdUtxoRefs of
      [r] -> pure r
      [] -> throwError . CheckError $ "No hold UTxO found"
      _ -> throwError . CheckError $ "Too many hold UTxOs found"
  logI'
    "Closing auction"
    [ "highest bid" .= highestBid,
      "thread token" .= threadToken
    ]
  let lookups =
        Constraints.unspentOutputs utxoMap
          <> Constraints.scriptInstanceLookups scrInst
          <> Constraints.otherScript scr
      payBacks = mconcat $ payBackBid threadUtxoMap <$> otherUtxoRefs
      payBackDatums = mconcat $ payBackBidDatums threadUtxoMap <$> otherUtxoRefs
      Just mustIncludeWinningDatum = mustIncludeDatum <$> txOutTxDatum winningTxOut
      constraints =
        validAfterDeadline (pEndTime params)
          -- FIXME Ugly
          <> payBacks
          <> mustPayOwner params winningUtxoRef highestBid
          <> mustTransferAsset params holdUtxoRef highestBid
          <> mustReturnThreadTokens threadTokensValue
          -- FIXME Workaround for missing input datum
          <> mustIncludeDatum (Datum $ PlutusTx.toData Hold)
          <> mustIncludeWinningDatum
          <> payBackDatums
  -- Submit tx
  ledgerTx <- submitTxConstraintsWith lookups constraints
  logInputs @ParallelAuctionState ledgerTx
  void . awaitTxConfirmed . txId $ ledgerTx
  -- Debug
  printUtxos' params
  pure ()
  where
    payBackBid utxoMap utxoRef =
      -- FIXME Unsafe
      let Just txOut = Map.lookup utxoRef utxoMap
          -- FIXME Unsafe
          Just b = txOutTxToBid txOut
          Just d = txOutTxDatum txOut
          pb = mustPayBackBid b
       in mustSpendScriptOutput utxoRef closeRedeemer
            <> pb
    payBackBidDatums utxoMap utxoRef =
      -- FIXME Unsafe
      let Just txOut = Map.lookup utxoRef utxoMap
          -- FIXME Unsafe
          Just d = txOutTxDatum txOut
       in mustIncludeDatum d

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
    step Nothing (ref, txOut) = Just (ref, txOut, [])
    step (Just (aref, aTxOut, arefs)) (ref, txOut) =
      case Haskell.compare (txOutTxToBid txOut) (txOutTxToBid aTxOut) of
        GT -> Just (ref, txOut, aref : arefs)
        _ -> Just (aref, aTxOut, ref : arefs)

-- | Selects any of the existing thread UTxO for placing own bid by spending this UTxO.
selectUtxoIndex :: PubKeyHash -> Integer -> Int
selectUtxoIndex pkHash threadCount = hash pkHash `mod` fromIntegral threadCount

-- | Compares expected thread count with count of bidding UTxO threads.
hasCorrectUtxoCount :: Integer -> Int -> Bool
hasCorrectUtxoCount threadCount utxoCount =
  fromIntegral threadCount Haskell.== utxoCount

txOutTxToBid :: TxOutTx -> Maybe Bid
txOutTxToBid o = txOutTxDatum o >>= datumToBidInBidding

datumToBidInBidding :: Datum -> Maybe Bid
datumToBidInBidding (Datum d) =
  PlutusTx.fromData @ParallelAuctionState d >>= selectBidInBidding

datumToHold :: Datum -> Maybe Bool
datumToHold (Datum d) =
  selectHoldInBidding <$> PlutusTx.fromData @ParallelAuctionState d

selectBidInBidding :: ParallelAuctionState -> Maybe Bid
selectBidInBidding (Bidding b) = Just b
selectBidInBidding _ = Nothing

selectHoldInBidding :: ParallelAuctionState -> Bool
selectHoldInBidding Hold = True
selectHoldInBidding _ = False

highestBidInUTxOs :: UtxoMap -> Maybe Bid
highestBidInUTxOs utxoMap = do
  maximumByOf
    ( folded
        . Control.Lens.to txOutTxDatum
        . _Just
        . Control.Lens.to datumToBidInBidding
        . _Just
    )
    Haskell.compare
    utxoMap

filterBiddingThreadUTxOs :: UtxoMap -> UtxoMap
filterBiddingThreadUTxOs utxoMap =
  Map.fromList $
    Map.toList utxoMap
      ^.. folded
        . filteredBy
          ( _2
              . Control.Lens.to txOutTxDatum
              . _Just
              . Control.Lens.to datumToBidInBidding
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
failWithIfFalse :: ParallelAuctionError -> Bool -> ParallelAuctionContract ()
failWithIfFalse e c = if Haskell.not c then throwError e else pure ()

failWithIfNothing :: ParallelAuctionError -> Maybe a -> ParallelAuctionContract a
failWithIfNothing e = \case
  Just a -> pure a
  Nothing -> throwError e

printUtxos' :: ParallelAuctionParams -> ParallelAuctionContract ()
printUtxos' p = printUtxos @ParallelAuctionState $ scrAddress p
