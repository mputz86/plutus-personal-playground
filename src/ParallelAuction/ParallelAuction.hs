{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
module ParallelAuction.ParallelAuction where

import Control.Lens (Ixed (ix), makeClassyPrisms, (^?))
import Control.Monad (Monad ((>>), (>>=)), void)
import Data.Aeson as Aeson (FromJSON, ToJSON, (.=), toJSON)
import Data.Hashable (hash)
import Data.Monoid (First (..))
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Ledger
  ( Ada,
    Address,
    Datum (Datum),
    PubKeyHash,
    Redeemer (Redeemer),
    ScriptContext (..),
    ScriptPurpose (Spending),
    Slot,
    TxOut (TxOut, txOutDatumHash, txOutValue),
    TxOutRef,
    Validator,
    Value,
    getContinuingOutputs,
    pubKeyHash,
    scriptAddress,
    txId,
    txOutDatum,
  )
import Ledger.Ada as Ada (Ada (Lovelace), adaSymbol, toValue)
import Ledger.Constraints as Constraints
  ( TxConstraints,
    checkScriptContext,
    mustIncludeDatum,
    mustPayToPubKey,
    mustPayToTheScript,
    mustSpendScriptOutput,
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
    ownPubKey,
    select,
    submitTxConstraints,
    submitTxConstraintsWith,
    throwError,
    utxoAt,
    type (.\/),
  )
import Plutus.Contract.Types (ContractError)
-- FIXME HLS fix: Comment for HLS to work
import Plutus.Contracts.Currency (AsCurrencyError (_CurrencyError), CurrencyError, forgeContract, forgedValue)
import qualified Plutus.V1.Ledger.Interval as Interval
import qualified Plutus.V1.Ledger.Value as Value
import qualified PlutusTx
import PlutusTx.AssocMap as AssocMap (delete, lookup, singleton)
import PlutusTx.Builtins (String)
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
    Ord (compare, (<)),
    Ordering (EQ),
    Show,
    fromIntegral,
    fromMaybe,
    isJust,
    length,
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
    (==),
  )
import Utils.LoggingUtil
  ( logI,
    logI',
    logInputs,
    logUtxos,
    toLogT,
    toLogT',
  )
import Utils.UtxoUtil
  ( UtxoAccessor (..),
    lookupData,
    newUtxoMapAccessor,
    newUtxoTxInfoAccessor,
  )
import Prelude (Semigroup (..))
import qualified Prelude as Haskell

--
--
-- Data
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

data ParallelAuctionState
  = -- | State which holds asset
    Hold
  | -- | State for bidding threads
    --   FIXME Should be sufficient to just store pub key hash of bidder since value is at UTxO
    Bidding {dHighestBid :: Bid}
  | -- | Auction was closed
    Finished
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''ParallelAuctionState

-- Inputs / Redeemer for progressing the auction
data ParallelAuctionInput
  = InputBid Bid
  | InputClose

PlutusTx.unstableMakeIsData ''ParallelAuctionInput

-- Accumulated UTxOs: Splits all valid UTxOs into a highest bidding UTxO,
-- all other bidding UTxOs and the seen hold UTxOs (but should be 1).
data ParallelAuctionUtxos = ParallelAuctionUtxos
  { highestBiddingUtxo :: Maybe (TxOutRef, TxOut, ParallelAuctionState),
    otherBiddingUtxos :: [(TxOutRef, TxOut, ParallelAuctionState)],
    holdUtxos :: [(TxOutRef, TxOut, ParallelAuctionState)]
  }

PlutusTx.unstableMakeIsData ''ParallelAuctionUtxos

--
--
-- Utxo accessor for off-chain and on-chain
--
--

{-# INLINEABLE defaultParallelAuctionUtxos #-}
defaultParallelAuctionUtxos :: ParallelAuctionUtxos
defaultParallelAuctionUtxos = ParallelAuctionUtxos Nothing [] []

{-# INLINEABLE accumulateUtxos #-}
accumulateUtxos :: UtxoAccessor a -> ParallelAuctionUtxos
accumulateUtxos a@UtxoAccessor {..} =
  aFoldF step defaultParallelAuctionUtxos
  where
    step r@ParallelAuctionUtxos {..} (oref, txOut) = fromMaybe r $ do
      dh <- txOutDatum txOut
      case lookupData a (oref, dh) of
        Just h@Hold ->
          Just $ r {holdUtxos = (oref, txOut, h) : holdUtxos}
        Just b@(Bidding (Bid newBid _)) ->
          case highestBiddingUtxo of
            -- No highest bid so far, set current bid as highest
            Nothing -> Just $ r {highestBiddingUtxo = Just (oref, txOut, b)}
            -- Check if new bid replaces highest bid
            Just w@(_, _, Bidding (Bid curBid _))
              | curBid < newBid ->
                Just $ r {highestBiddingUtxo = Just (oref, txOut, b), otherBiddingUtxos = w : otherBiddingUtxos}
            -- Otherwise, just add bid to other bids
            _ -> Just $ r {otherBiddingUtxos = (oref, txOut, b) : otherBiddingUtxos}
        _ -> Nothing

--
--
-- Utility functions for validation
-- FIXME Can they be put in another module? Should work, but does not.
--
--

{-# INLINEABLE traceWithIfNothing' #-}
traceWithIfNothing' :: PlutusTx.Builtins.String -> Maybe a -> Maybe a
traceWithIfNothing' s = \case
  Nothing -> trace s Nothing
  Just v -> pure v

{-# INLINEABLE traceWithIfFalse' #-}
traceWithIfFalse' :: PlutusTx.Builtins.String -> Bool -> Maybe ()
traceWithIfFalse' s v = if not v then trace s Nothing else pure ()

-- | Untyped redeemer for 'InputClose' input.
{-# INLINEABLE closeRedeemer #-}
closeRedeemer :: Redeemer
closeRedeemer = Redeemer $ PlutusTx.toData InputClose

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

{-# INLINEABLE extractThreadToken' #-}
extractThreadToken' :: TxOut -> Maybe Value
extractThreadToken' = snd . splitNativeAndThreadToken . txOutValue

--
--
-- Utility functions for extracting values out of UTxOs
--
--

{-# INLINEABLE getBiddingThreadCount #-}
getBiddingThreadCount :: ParallelAuctionUtxos -> Integer
getBiddingThreadCount ParallelAuctionUtxos {..} = length otherBiddingUtxos + length highestBiddingUtxo

{-# INLINEABLE extractSingleHold #-}
extractSingleHold :: ParallelAuctionUtxos -> Maybe (TxOutRef, TxOut, ParallelAuctionState)
extractSingleHold ParallelAuctionUtxos {..} =
  case holdUtxos of
    [r] -> pure r
    _ -> Nothing

{-# INLINEABLE extractBid #-}
extractBid :: ParallelAuctionState -> Maybe Bid
extractBid = \case
  Bidding b -> Just b
  _ -> Nothing

{-# INLINEABLE extractHighestBid #-}
extractHighestBid :: ParallelAuctionUtxos -> Maybe (TxOutRef, TxOut, ParallelAuctionState, Bid)
extractHighestBid ParallelAuctionUtxos {highestBiddingUtxo} = do
  (r, o, s) <- highestBiddingUtxo
  b <- extractBid s
  pure (r, o, s, b)

--
--
-- Utility functions for checking conditions based on UTxOs
--
--

{-# INLINEABLE checkHasBiddingThreadCount #-}
checkHasBiddingThreadCount :: ParallelAuctionUtxos -> Integer -> Bool
checkHasBiddingThreadCount utxos = (==) (getBiddingThreadCount utxos)

-- | Requires the new bid (first arg) to be higher
{-# INLINEABLE checkHasHigherBid #-}
checkHasHigherBid :: Bid -> Bid -> Bool
checkHasHigherBid (Bid newBid _) (Bid oldBid _) = oldBid < newBid

--
--
-- Constraints, checked in off- and on-chain code.
--
--

{-# INLINEABLE mustBeValidAfterDeadline #-}
mustBeValidAfterDeadline :: Slot -> TxConstraints i o
mustBeValidAfterDeadline deadline = mustValidateIn (Interval.from deadline)

{-# INLINEABLE mustBeValidBeforeDeadline #-}
mustBeValidBeforeDeadline :: Slot -> TxConstraints i o
mustBeValidBeforeDeadline deadline =
  -- FIXME @Lars: Using (Enum.pred deadline) leads to failing on-chain code. Intended?
  mustValidateIn (Interval.to $ deadline - 1)

{-# INLINEABLE mustDistributeThreadTokensWithInitBid #-}
mustDistributeThreadTokensWithInitBid :: Bid -> [Value] -> TxConstraints i ParallelAuctionState
mustDistributeThreadTokensWithInitBid initialBid tokenValues =
  mconcat $ fmap (mustPayToTheScript $ Bidding initialBid) tokenValues

{-# INLINEABLE mustPayAssetFromOwnerToScript #-}
mustPayAssetFromOwnerToScript :: Value -> TxConstraints i ParallelAuctionState
mustPayAssetFromOwnerToScript = mustPayToTheScript Hold

{-# INLINEABLE mustPayOwner #-}
mustPayOwner :: ParallelAuctionParams -> TxOutRef -> Bid -> TxConstraints i o
mustPayOwner params highestBidUtxoRef highestBid =
  mustSpendScriptOutput highestBidUtxoRef closeRedeemer
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
mustPayBackBid oldBid =
  mustPayToPubKey (bBidder oldBid) (Ada.toValue $ bBid oldBid)

{-# INLINEABLE mustPayBackOtherBids #-}
mustPayBackOtherBids :: ParallelAuctionUtxos -> TxConstraints i ParallelAuctionState
mustPayBackOtherBids ParallelAuctionUtxos {..} =
  mconcat $ payBack <$> otherBiddingUtxos
  where
    -- FIXME Check if 'mustSpendScriptOutput' should be added to 'mustPayBackBid'
    payBack (oref, _, Bidding b) =
      mustPayBackBid b
        <> mustSpendScriptOutput oref closeRedeemer
    payBack _ = mempty

-- | Version of 'checkScriptContext' with fixed types for this contract.
{-# INLINEABLE checkConstraints #-}
checkConstraints :: TxConstraints ParallelAuctionInput ParallelAuctionState -> ScriptContext -> Bool
checkConstraints = checkScriptContext @ParallelAuctionInput @ParallelAuctionState

--
--
-- On-Chain validation
--
--

{-# INLINEABLE validateNewBid #-}
validateNewBid :: ParallelAuctionParams -> ScriptContext -> Bid -> Bid -> Bool
validateNewBid params ctx@ScriptContext {scriptContextPurpose = Spending txOutRef} curBid newBid = isJust $ do
  -- Ensure new bid is higher than current
  traceWithIfFalse'
    "New bid is not higher"
    (checkHasHigherBid newBid curBid)
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
    (checkConstraints (mustBeValidBeforeDeadline $ pEndTime params) ctx)
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

-- | Used to validate if bidding threads and hold UTxO is consumed by auction closing tx.
{-# INLINEABLE validateIsClosingTx #-}
validateIsClosingTx :: ParallelAuctionParams -> ScriptContext -> Bool
validateIsClosingTx params@ParallelAuctionParams {..} ctx@ScriptContext {scriptContextTxInfo = txInfo} = isJust $ do
  let utxoAccessor = newUtxoTxInfoAccessor txInfo
      utxos@(ParallelAuctionUtxos _ _ _) = accumulateUtxos @ParallelAuctionState utxoAccessor
  -- Tx construction check
  (holdUtxoRef, _, _) <-
    traceWithIfNothing'
      "Consumes not exactly 1 hold state"
      $ extractSingleHold utxos
  traceWithIfFalse'
    "Consumes not exactly as many bidding threads as threads"
    $ checkHasBiddingThreadCount utxos pThreadCount
  -- UTxO inputs check (and value extraction)
  (highestBidUtxoRef, highestBidTxOut, _, highestBid) <-
    traceWithIfNothing'
      "Failed to find highest bid"
      $ extractHighestBid utxos
  threadToken <-
    traceWithIfNothing' "Failed to find thread token" $
      extractThreadToken' highestBidTxOut
  let totalThreadTokensValue = Value.scale pThreadCount threadToken
  -- Tx constraints checks
  traceWithIfFalse'
    "Closing tx only allowed after deadline"
    $ checkConstraints (mustBeValidAfterDeadline pEndTime) ctx
  traceWithIfFalse'
    "Does not pay back other bids"
    $ checkConstraints (mustPayBackOtherBids utxos) ctx
  traceWithIfFalse'
    "Does not pay back to owner"
    $ checkConstraints (mustPayOwner params highestBidUtxoRef highestBid) ctx
  traceWithIfFalse'
    "Does not transfer asset to highest bidder"
    $ checkConstraints (mustTransferAsset params holdUtxoRef highestBid) ctx
  traceWithIfFalse'
    "Does not return all thread tokens to contract"
    $ checkConstraints (mustReturnThreadTokens totalThreadTokensValue) ctx

--
--
-- On-Chain validator: In essence a state machine
--
--

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

--
--
-- Off-chain contract and endpoints
--
--

-- | Contract Errors
data ParallelAuctionError
  = TContractError ContractError
  | -- FIXME HLS fix: Comment for HLS to work
    TCurrencyError CurrencyError
  | CheckError Text.Text
  deriving stock (Haskell.Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''ParallelAuctionError

-- FIXME HLS fix: Comment for HLS to work
instance AsCurrencyError ParallelAuctionError where
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
  logI "Starting auction"
  ownPkHash <- pubKeyHash <$> ownPubKey
  -- Create tokens for bidding threads
  threadTokenValues <- createBiddingThreads ownPkHash (pThreadCount params)
  -- Create tx: One UTxO per bidding thread (state Bidding) and one UTxO with asset (state Hold)
  let scrInst = inst params
      constraints =
        mustDistributeThreadTokensWithInitBid (Bid 0 ownPkHash) threadTokenValues
          <> mustPayAssetFromOwnerToScript (pAsset params)
  ledgerTx <- submitTxConstraints scrInst constraints
  void . awaitTxConfirmed . txId $ ledgerTx
  -- Debug
  logUtxos' params

-- | Places bid
bid :: (ParallelAuctionParams, Integer) -> ParallelAuctionContract ()
bid (params@ParallelAuctionParams {..}, bidAmount) = do
  ownPkHash <- pubKeyHash <$> ownPubKey
  let scrInst = inst params
      scr = validator params
      scrAddr = scrAddress params
      ownBid = Bid (Ada.Lovelace bidAmount) ownPkHash
  utxoMap <- utxoAt scrAddr
  -- Aggregate all UTxOs
  let utxoAccessor = newUtxoMapAccessor utxoMap
      utxos@(ParallelAuctionUtxos _ _ _) = accumulateUtxos utxoAccessor
  -- Checks for UTxOs
  checkBiddingThreadCount utxos pThreadCount
  -- Check for highest bid and if own bid is higher
  (_, _, _, highestBid) <-
    checkHighestBid utxos
  checkOwnBidHighest ownBid highestBid
  -- Select any bidding UTxO thread to place own bid
  (utxoIndex, utxoBidRef, oldBidTxOut, oldThreadBidding) <-
    checkSelectUtxo utxos ownPkHash
  threadToken <- checkThreadToken oldBidTxOut
  oldThreadBid <- checkOldThreadBid oldThreadBidding
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
        mustBeValidBeforeDeadline pEndTime
          <> mustPayBackBid oldThreadBid
          <> mustUseThreadTokenAndPayBid utxoBidRef threadToken ownBid
  -- Submit tx
  ledgerTx <- submitTxConstraintsWith lookups constraints
  -- Debug: Log inputs
  logInputs @ParallelAuctionState ledgerTx
  void . awaitTxConfirmed . txId $ ledgerTx
  logUtxos' params
  where
    checkOwnBidHighest ownBid highestBid =
      failWithIfFalse
        ( CheckError $
            toLogT'
              "Failed since another bid is higher than own bid"
              [ "own bid" .= ownBid,
                "highest bid" .= highestBid
              ]
        )
        (checkHasHigherBid ownBid highestBid)
    checkSelectUtxo utxos pkHash =
      failWithIfNothing
        (CheckError "Could not find an UTxO index to place bid")
        $ selectUtxoAtIndex utxos pkHash
    checkOldThreadBid oldThreadBidding =
      failWithIfNothing
        (CheckError "No old bid found in bidding thread")
        $ extractBid oldThreadBidding

-- | Closes auction
close :: ParallelAuctionParams -> ParallelAuctionContract ()
close params@ParallelAuctionParams {..} = do
  let scrInst = inst params
      scr = validator params
      scrAddr = scrAddress params
  utxoMap <- utxoAt scrAddr
  -- Aggregate all UTxOs
  let utxoAccessor = newUtxoMapAccessor utxoMap
      utxos@(ParallelAuctionUtxos _ otherBids _) = accumulateUtxos utxoAccessor
  checkBiddingThreadCount utxos pThreadCount
  -- Select the UTxO with the highest bid
  (highestBidUtxoRef, highestBidTxOut, highestBidding, highestBid) <-
    checkHighestBid utxos
  threadToken <- checkThreadToken highestBidTxOut
  let totalThreadTokensValue = Value.scale pThreadCount threadToken
  (holdUtxoRef, _, _) <- checkHoldState utxos
  logI'
    "Closing auction"
    [ "highest bid" .= highestBid,
      "thread token" .= threadToken
    ]
  let lookups =
        Constraints.unspentOutputs utxoMap
          <> Constraints.scriptInstanceLookups scrInst
          <> Constraints.otherScript scr
      -- FIXME Workaround for missing input datums; to be removed with fix.
      mustIncludeHighestBidDatum = mustIncludeDatum . Datum . PlutusTx.toData @ParallelAuctionState $ highestBidding
      mustIncludeHoldDatum = mustIncludeDatum (Datum $ PlutusTx.toData Hold)
      mustIncludePayBackBidsDatums = mconcat $ mustIncludePayBackBidDatum utxoAccessor <$> otherBids
      mustIncludeMissingInputDatums =
        mustIncludeHighestBidDatum <> mustIncludeHoldDatum <> mustIncludePayBackBidsDatums
      constraints =
        mustBeValidAfterDeadline pEndTime
          <> mustPayBackOtherBids utxos
          <> mustPayOwner params highestBidUtxoRef highestBid
          <> mustTransferAsset params holdUtxoRef highestBid
          <> mustReturnThreadTokens totalThreadTokensValue
          <> mustIncludeMissingInputDatums
  -- Submit tx
  ledgerTx <- submitTxConstraintsWith lookups constraints
  -- Debug: Log inputs
  logInputs @ParallelAuctionState ledgerTx
  void . awaitTxConfirmed . txId $ ledgerTx
  where
    checkHoldState utxos =
      failWithIfNothing
        (CheckError "Failed to find exactly one hold state")
        $ extractSingleHold utxos
    mustIncludePayBackBidDatum a (oref, txOut, _) = fromMaybe mempty $ do
      dh <- txOutDatumHash txOut
      d <- lookupData a (oref, dh)
      pure $ mustIncludeDatum d

-- Off-chain checks
checkBiddingThreadCount :: ParallelAuctionUtxos -> Integer -> ParallelAuctionContract ()
checkBiddingThreadCount utxos threadCount = do
  failWithIfFalse
    ( CheckError $
        toLogT
          "Failed since wrong thread UTxO count"
          [ "thread count" .= threadCount,
            "utxo count" .= getBiddingThreadCount utxos
          ]
    )
    (checkHasBiddingThreadCount utxos threadCount)

checkHighestBid :: ParallelAuctionUtxos -> ParallelAuctionContract (TxOutRef, TxOut, ParallelAuctionState, Bid)
checkHighestBid utxos = do
  failWithIfNothing
    (CheckError "Failed to find highest bid")
    (extractHighestBid utxos)

checkThreadToken :: TxOut -> ParallelAuctionContract Value
checkThreadToken txOut =
  failWithIfNothing
    (CheckError "No thread token found in thread")
    $ extractThreadToken' txOut

--
--
-- Off-chain utility functions
--
--

createBiddingThreads ::
  PubKeyHash ->
  Integer ->
  ParallelAuctionContract [Value]
createBiddingThreads pkHash threadCount = do
  -- FIXME HLS fix: Uncomment for HLS to work
  -- Haskell.undefined
  -- FIXME HLS fix: Comment for HLS to work
  c <- forgeContract pkHash [("auction-threads", fromIntegral threadCount)]
  pure . toSingleValues . forgedValue $ c

toSingleValues :: Value -> [Value]
toSingleValues v = do
  (s, tn, amt) <- Value.flattenValue v
  replicate (fromIntegral amt) $ Value.singleton s tn 1

-- | Returns the UTxO which is selected based on pub key hash and size of availabe bidding threads.
selectUtxoAtIndex :: ParallelAuctionUtxos -> PubKeyHash -> Maybe (Int, TxOutRef, TxOut, ParallelAuctionState)
selectUtxoAtIndex utxos@ParallelAuctionUtxos {..} pkHash =
  let threadCount = getBiddingThreadCount utxos
      idx = selectUtxoIndex pkHash threadCount
      -- Select either from other bidding threads or the highest
      First utxo =
        First (otherBiddingUtxos ^? ix idx)
          <> First highestBiddingUtxo
   in case utxo of
        Just (r, o, s) -> Just (idx, r, o, s)
        Nothing -> Nothing

selectUtxoIndex :: PubKeyHash -> Integer -> Int
selectUtxoIndex pkHash threadCount = hash pkHash `mod` fromIntegral threadCount

-- General Helper
failWithIfFalse :: ParallelAuctionError -> Bool -> ParallelAuctionContract ()
failWithIfFalse e c = if Haskell.not c then throwError e else pure ()

failWithIfNothing :: ParallelAuctionError -> Maybe a -> ParallelAuctionContract a
failWithIfNothing e = \case
  Just a -> pure a
  Nothing -> throwError e

logUtxos' :: ParallelAuctionParams -> ParallelAuctionContract ()
logUtxos' p = logUtxos @ParallelAuctionState $ scrAddress p
