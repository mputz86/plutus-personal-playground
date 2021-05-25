
# Details and Learnings: Parallel Auction


Source code: [src/ParallelAuction/ParallelAuction.hs](src/ParallelAuction/ParallelAuction.hs).


## Idea

Idea is to try out if bidding in an auction can be done in parallel by using different UTxOs if multiple bids should be placed within the same slot.

Original Problem:
- `Auction` example in `plutus-use-cases` uses `StateMachine`
- Can only process one bid per slot

Solution:
- Provide multiple threads, i.e. UTxOs, where bidders can place their bids
- Which UTxO is determined by the pub key has of the bidder
- Process these bids in parallel
- After deadline: Combine all threads and determine final winner

More information on the idea and impl in the source file: [src/ParallelAuction/ParallelAuction.hs](src/ParallelAuction/ParallelAuction.hs).



## State And Usage

Lacks probably still a lot of corner cases. Focus was rather to get a grip on all the data, the flow, a half-way nice style of coding, ... .

Using it:
- Use [src/ParallelAuction/ParallelAuctionTrace.hs](src/ParallelAuction/ParallelAuctionTrace.hs) in `cabal repl`
- Run `testParallelBidding`, `testSequentialBidding` or `testSimpleBidding`



## Outcomes

- [LoggingUtil](../src/Utils/LoggingUtil.hs)
  - Colorful, json structured, unified logging for `EmulatorTrace` and `Contract`.
- [UtxoAccessor](../src/Utils/UtxoAccessor.hs)
  - Unified access for on- and off-chain code in order to create `TxConstraints` more easily.
  - TODO Rewrite as type-class



## Learnings

- It is essential to define data types before doing anything else
  - Contract params, `ParallelAuctionParams`
  - State / `Datum`, `ParallelAuctionState`
  - Inputs / `Redeemer`, `ParallelAuctionInput`
  - Especially:
    - Think about what belongs to contract params, what to `Datum`
    - Think about `Datum` and `Redeemer` as state and input: So in essence validation is checking for valid transitions in a given state

- Also: Sketching the idea with possible transitions helps a lot
  - Note: Use symbols which Lars uses
    - Boxes for transactions
    - Red circles for UTxOs
    - Blue, connected circles, for consumed UTxOs
    - Mark UTxOs with a Name/Address it belongs to

- Never forget that anybody can send money to any address

- Defining valid transactions
  - Constraints defined as `TxConstraints` can be used on- and off-chain
    - On-chain with `checkScriptContext`
    - Off-chain by submitting with, e.g., `submitTxConstraints`
  - But still: Some checks can not be expressed with constraints

- `TxConstraints` must be created on- and off-chain
  - But: Access to UTxOs is different for on-chain and off-chain code
  - Solution: [UtxoAccessor](../src/Utils/UtxoAccessor.hs)
  - Can be easily created on- and off-chain
  - Provides unified access to UTxOs
    - Provides a way to fold over the UTxOs in order to gather relevant information for validation
  - E.g. `accumulateUtxos` expects an `UtxoAccessor` and returns with `ParallelAuctionUtxos` a data type with all information required to create the constraints
  - Creating accessor off-chain, in `Contract` monad
    ```haskell
  utxoMap <- utxoAt scrAddr
  -- Aggregate all UTxOs
  let utxoAccessor = newUtxoMapAccessor utxoMap
      utxos@(ParallelAuctionUtxos _ _ _) = accumulateUtxos utxoAccessor
    ```

  - Creating accessor on-chain (`txInfo` is from the `ScriptContext`)
```haskell
  let utxoAccessor = newUtxoTxInfoAccessor txInfo
      utxos@(ParallelAuctionUtxos _ _ _) = accumulateUtxos @ParallelAuctionState utxoAccessor
```

- Naming for validation functions
  - Name own `TxConstraints` by starting with `mustXXX`, similar to provided constraints
    - E.g. `mustPayAssetFromOwnerToScript`
  - Name checks with `checkXXX` and return a `Bool`
    - E.g. `checkHasHigherBid`
  - Name value/info extraction `extractXXX` and return a `Maybe a`
    - E.g. `extractBid`

- On-chain code
  - Use `Maybe` monad for validation
    - Avoids cascading
  - Use `isJust` to convert to final output for validator
  - Use `traceWithIfNothing'` and `traceWithIfFalse'` to exit with tracing if a condition fails
  - See e.g. `validateNewBid`
```haskell
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
  -- ...
```
  - TODO: Think about using `Either`

- Off-chain code
  - Similar to `Maybe`, use `throwError` for early exists and non-cascading checks
  - Use helper functions `failWithIfNothing` and `failWithIfFalse`
  - E.g. `bid`
```haskell
-- | Places bid
bid :: (ParallelAuctionParams, Integer) -> ParallelAuctionContract ()
bid (params@ParallelAuctionParams {..}, bidAmount) = do
  -- ...
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

```

- Different errors like `ContractError` or `CurrencyError` can be combined by providing own error, `ParallelAuctionError`
```haskell
data ParallelAuctionError
  = TContractError ContractError
  | TCurrencyError CurrencyError
  | CheckError Text.Text
  deriving stock (Haskell.Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
```
  - Plus some more boiler plate

- Logging
  - Needs to be readable
  - Created [LoggingUtil](../src/Utils/LoggingUtil.hs)
  - Enhanced logging with `EmulatorTrace`
    - Provides with `showEventPretty` a replacement for the default `showEvent` in `TraceConfig`.
    - Prints out pretty, colorful JSONs to terminal (and repl)
    - See the screenshot for how it looks like.

   - Unification of logging in `Contract` and `EmulatorTrace` monad; i.e. there are methods for both monads which can be used in the same way:
    - `logI`, `logD`, ... for logging single strings and
    - `logI'`, ... for logging a title along with key values.
    - eg
  ```haskell
  logI'
    "UTxOs"
    [ "script address" .= scrAddr,
      "UTxO count" .= Map.size utxoMap,
      "UTxO datums" .= datums
    ]
  ```
  - Some helper functions for logging UTxOs in `Contract`: For now `logInputs`, `logUtxos`, `logUtxoCount`


- Importing `Currency` (copied from `plutus-use-cases`) breaks HLS in editor
  - Unsolved
  - Probably has something to do with unused imports of modules with Template Haskell?
  - Maybe https://github.com/haskell/haskell-language-server/issues/1705
