---
date: 2021-05-16T23:27
---

# Cardano Plutus Pioneer Program: Lecture 6

[Lecture Video](https://www.youtube.com/watch?v=wY7R-PJn66g)

- Fully fledged Dapp (run on mockchain)
- Contains all pieces for real apps
- Example: Oracles
  - "Service"/Way to get real world data on blockchain
  - Make it usable in smart contracts
  - Like: Weather data, election results, stock exchanges, randomness, ...
  - E.g. betting on result of a sport game
- Here: One feed of data
  - Exchange rate of USD-to-Dollar
  - Risk: Need to rely on the source for this data
    - May fail to provide data
    - May provide wrong data
    - ...
  - Ways to migitate:
    - Make provider put down collateral: If provider fails to provide data, it looses this collateral
    - Or: Combine multiple oracles
    - Only accept if all agree, or take average, ...
  - For example: One provider which we trust


## Idea of Oracle

- Use UTxO: Oracle value sits at script address of Oracle
- Problem
  - Validation only happens on consumption
  - I.e. cannot prevent anybody from providing arbitrary output at same Oracle script address
  - => Need a way to distinguish true UTxO from others
  - Put NFT value on true Oracle UTxO

- How can it be used?
  - Until now: Knew all ways to use contract upfront
  - Here: Different, dont know how Oracle will be used
  - So an open API must be provided
  - I.e. must be usable by smart contracts which do not exist at the time when the Oracle is created

## Swap Contract: ADA to USD

- Incentive for Oracle to create data:
  - Puts fee whenever data is used (e.g. 1 ADA)
- Example:
  - Swap 100 ADA to USD (Seller)
  - Buyer pays fee and gets ADA

- Transaction
  - Spents Oracle UTxO (with NFT)
  - Validator `use` needs to check
    - NFT present in UTxO
    - Exists output at same Oracle address,
    - also containing NFT and same Oracle value (`Datum`)
    - Fee must be paied to Oracle
  - Validator `swap` needs to check
    - Consume UTxO of ADA-Seller with ADA
    - Buyer UTxO for providing USD and paying fees
    - Transaction output to seller: USDs
    - Transaction output to buyer: ADA

- Note: `swap` just one example, there will be much more use cases of the Oracle

- Also: Need to be a way for price of ADA-to-USD to be updated
  - Other Oracles, like result of sports match, are single events in history
    - So not all Oracles need update functionality
  - But here: Provider must be able to change value
  - Transaction must
    - consume UTxO and provide new one which provides updated UTxO
    - Plus pay fee to owner
    - Must be signed by Oracle provider

## Module `Oracle.Core`

- Oracle: Parametrized Contract
```haskell
data Oracle = Oracle
    { oSymbol   :: !CurrencySymbol -- ^ Symbol of NFT, empty string as TokenName
    , oOperator :: !PubKeyHash     -- ^ Owner / Operator of Oracle (can do updates, gets fees)
    , oFee      :: !Integer        -- ^ Fees in lovelaces for data usage
    , oAsset    :: !AssetClass     -- ^ Exchange from ADA (e.g. USD)
    } -- ...
```

- Redeemer: Updates and Use
```haskell
data OracleRedeemer = Update | Use
```

- Note: Use `Integer` to represent types
  - There is a `Ratio` type in Plutus, but has some issues
  - Easier to use `Integer` with 1mio unit: 1.75 = 1'750'000


- Most important function: `mkOracleValidator`
```haskell
mkOracleValidator
  :: Oracle          -- ^ Parametrization
  -> Integer         -- ^ Value, current exchange rate
  -> OracleRedeemer  -- ^ Redeemer,so use or update (share some checks)
  -> ScriptContext
  -> Bool
```
  - Common checks for use and update
    - Exists input which uses Oracle NFT
      - Check that NFT exists exactly once in UTxO input
    - Exists output which spents Oracle NFT
      - Uses library function `getContinuingOutputs`
      - Contains list of outputs which go to same script address which is currently validated
      - Assumption: Must be exactly one
      - Check if in this output is the NFT exactly once present
  - Checks for update
    - Operator has signed the transaction
    - Must contain a valid output `Datum`: Checks by looking up value of output
    - Note: No further checks, so operator can pay fees to himself
  - Checks for use
    - Prohibit datum to change
    - Ensure fees have been paid
      - Check that own outputs are greater or equals to
      - own inputs, plus fees
      - By doing "greater or equals": User of Oracle can pay a tip

- Off-chain code
  - Only start and update oracle
  - Not usage: This is not responsibility of Oracle provider
    - I.e. on-chain code with Redeemer `Use` is not reflected in off-chain code

  - Start:
    - Params for Oracle, `OracleParams`
    - No initial value, since creating NFT takes some slots
      - I.e. exchange rate may be outdated already
    - Mint NFT (only thing done in start)
      - Use `forgeContract` from `Currency` module (in `plutus-use-cases`)
        ```haskell
        forgeContract :: PubKeyHash -> [(TokenName, Integer)] -> Contract w s e OneShotCurrency
        ```
      - Note: Requires to map the `Contract` monad's error messages to `Text` with `mapError`
  - Update:
    - Must handle case where no UTxO exist yet
      - Done by using `findOracle`
      - Checks if at script address exists an UTxO with NFT
      - Returns a `Maybe` to signal if not present (Oracle just started)
    - If no UTxO found
      - Create UTxO at address with first exchange rate
      - `mustPayToTheScript` , with NFT and `Datum`
    - If UTxO exists
      - Existing UTxO must be spent, `mustSpendScriptOutput`
      - Requires lookups
        - `unspentOutputs` to find the UTxO ref which needs to be spent
        - Must provide script instances for input side (`scriptInstanceLookups`) and output side (`otherScript`)
      - Note: Balancing the transaction, will lead to pay fees from script to operator.
        - Since in input NFT with value, but on output only NFT
        - So inbalance of the payed fees (from Oracle users)
        - Balancing algorithm defaults to paying fees to own wallet
      - Also creates input which pays for transation fees

- Run Oracle function
  - Has endpoint `update`
  - Starts Oracle (i.e. mints NFTs)
  - And writes oracle value
  - Use `tell` to communiate the just created Oracle to the outside world
    - Note: Uses `Last` since `tell` requires a monoid
    - Monoid operation which remembers last (Just) value
      ```haskell
      Last (Just 'x') <> Last (Just 'y')
      > Last { getLast = Just 'y' }
      Last (Just 'x') <> Last Nothing
      > Last { getLast = Just 'x' }
      Nothing <> Last Nothing
      > Last { getLast = Nothing }
      ```
  - Loops forever, waits for updates


## Example Contract Swap, uses Oracle

- Idea
  - Somebody can put ADA in smart contract
  - Somebody can exchange ADA for another token (here USD)
  - Price will be determined by using Oracle, so changes over time

- In `Oracle.Swap`

- Validator `mkSwapValidator`
  - Takes two params:
    - `Oracle` (from Oracle module)
    - `Address` (of Oracle)
      - Normaly, given `Oracle`, address can be computed with a function
      - But this function not usable in validator, i.e. can not be compiled to Plutus
      - So explicitly hand in `Address`
  - Datum is `PubKeyHash` of seller
  - No redeemer
- Checks in validator
  - Three inputs
    - Script input from Oracle for exchange rate
    - Script input from swap contract, holds ADA/lovelaces
    - Funds of buyer
  - Three outputs
    - Oracle output, but Oracle cares about it
    - Seller must get tokens
    - Buyer must get ADA
  - Other use case: Seller can retrieve back his ADAs
    - If nobody does swap
    - Otherwise money locked forever
- Implementation for seller retrieves his ADA: Just check if tx is signed by seller, `txSignedBy info pkh`
- Implementation for swap
  - Check exactly two script inputs (avoid interference with other smart contracts)
    - Filter for script inputs and check if there are 2
      ```haskell
      hasTwoScriptInputs :: Bool
      hasTwoScriptInputs =
        let
          xs = filter (isJust . toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
        in
          length xs == 2

      ```
      - Note: `toValidatorHash` returns `Nothing` if no script output
  - All other inputs must be PubKey inputs
  - Check if seller is paid
    - Note: Validation script of Oracle will run and check that NFT is present in input
    - Get Oracle input, i.e. exchange rate
      - Extract inputs of Oracle
        ```haskell
        oracleInput :: TxOut
        oracleInput =
          let
            ins = [ o
                  | i <- txInfoInputs info
                  , let o = txInInfoResolved i
                  , txOutAddress o == addr
                  ]
          in
            case ins of
                [o] -> o
                _   -> traceError "expected exactly one oracle input"
        ```
      - Extract value from Oracle input (of type `Integer`)
        ```haskell
        oracleValue' = case oracleValue oracleInput (`findDatum` info) of
            Nothing -> traceError "oracle value not found"
            Just x  -> x
        ```
    - Find out how many lovelaces are locked in swap
      ```haskell
      minPrice :: Integer
      minPrice =
        let
          lovelaceIn = case findOwnInput ctx of
              Nothing -> traceError "own input not found"
              Just i  -> lovelaces $ txOutValue $ txInInfoResolved i
        in
          price lovelaceIn oracleValue'
      ```
      - Uses `price` function, to compute price based on available ADAs and exchange rate
    - Finally, check if seller gets paid
      - Sum up all outputs that go to public key address with `valuePaidTo info pkh`
      - Check for asset of swap: In our case, USD asset, which Oracle is used for
        ```haskell
        sellerPaid :: Bool
        sellerPaid =
          let
            pricePaid :: Integer
            pricePaid =  assetClassValueOf (valuePaidTo info pkh) (oAsset oracle)
          in
            pricePaid >= minPrice
        ```

- Offer swap contract: `offerSwap`
  - For seller, want to provide swap
  - Takes Oracle to use and lovelaces seller wants to add
  - Pays amout of lovelaces to script (swap)
  ```haskell
  offerSwap :: forall w s. HasBlockchainActions s => Oracle -> Integer -> Contract w s Text ()
  offerSwap oracle amt = do
      pkh <- pubKeyHash <$> Contract.ownPubKey
      let tx = Constraints.mustPayToTheScript pkh $ Ada.lovelaceValueOf amt
      ledgerTx <- submitTxConstraints (swapInst oracle) tx
      awaitTxConfirmed $ txId ledgerTx
      logInfo @String $ "offered " ++ show amt ++ " lovelace for swap"
  ```
- Helper function which returns all swaps: `findSwaps`
  - Which fulfill a predicate `(PubKeyHash -> Bool)`
  - Returns list of UTxOs sit at swap address, `TxOutRef`, `TxOutTx` and Datum (`PubKeyHash`)
    ```haskell
    findSwaps :: HasBlockchainActions s => Oracle -> (PubKeyHash -> Bool) -> Contract w s Text [(TxOutRef, TxOutTx, PubKeyHash)]
    findSwaps oracle p = do
        utxos <- utxoAt $ swapAddress oracle
        return $ mapMaybe g $ Map.toList utxos
      where
        f :: TxOutTx -> Maybe PubKeyHash
        f o = do
            dh        <- txOutDatumHash $ txOutTxOut o
            (Datum d) <- Map.lookup dh $ txData $ txOutTxTx o
            PlutusTx.fromData d

        g :: (TxOutRef, TxOutTx) -> Maybe (TxOutRef, TxOutTx, PubKeyHash)
        g (oref, o) = do
            pkh <- f o
            guard $ p pkh
            return (oref, o, pkh)
    ```
    - Note:
      - `mapMaybe :: (a -> Maybe b) -> [a] -> [b]`
      - `guard`: Checks predicate and fails if predicate not `True`

- Retrieving swaps: `retrieveSwaps`
  - Find swaps which belongs to one self
  - If there are some, return all by paying back from script
  ```haskell
  retrieveSwaps :: HasBlockchainActions s => Oracle -> Contract w s Text ()
  retrieveSwaps oracle = do
      pkh <- pubKeyHash <$> ownPubKey
      xs <- findSwaps oracle (== pkh)
      case xs of
          [] -> logInfo @String "no swaps found"
          _  -> do
              let lookups = Constraints.unspentOutputs (Map.fromList [(oref, o) | (oref, o, _) <- xs]) <>
                            Constraints.otherScript (swapValidator oracle)
                  tx      = mconcat [Constraints.mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | (oref, _, _) <- xs]
              ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
              awaitTxConfirmed $ txId ledgerTx
              logInfo @String $ "retrieved " ++ show (length xs) ++ " swap(s)"

  ```

- Make swap (buyer): `useSwap`
  - `ownFunds` helper function: Gets own funds and adds them up
    - Used for checking how much USD tokens must be paied
  - Find Oracle which allows swap with `findOracle`
  - Search for swaps which are not ours with `findSwaps oracle (/= pkh)`
    - Filter for swaps which can be done with own funds, which we can afford
    - `find (f amt x) swaps`
  - If one: Take first one (too make things simpler)
  - Create transaction
    - Output for oracle:
      - Existing output of Oracle, add own fees `<> lovelaceValueOf (oFee oracle)`
    - Compute price of asset to pay `p`
    - Constraints:
      - Spent Oracle output with `Use` redeemer
      - Spent swap output (no redeemer)
      - Pay fee to oracle
      - Pay seller of lovelace
    - Add lookups to work:
      - Swap validator,
      - Oracle validator,
      - UTxOs to consume of oracle and swap
  ```haskell
  useSwap :: forall w s. HasBlockchainActions s => Oracle -> Contract w s Text ()
  useSwap oracle = do
      funds <- ownFunds
      let amt = assetClassValueOf funds $ oAsset oracle
      logInfo @String $ "available assets: " ++ show amt

      m <- findOracle oracle
      case m of
          Nothing           -> logInfo @String "oracle not found"
          Just (oref, o, x) -> do
              logInfo @String $ "found oracle, exchange rate " ++ show x
              pkh   <- pubKeyHash <$> Contract.ownPubKey
              swaps <- findSwaps oracle (/= pkh)
              case find (f amt x) swaps of
                  Nothing                -> logInfo @String "no suitable swap found"
                  Just (oref', o', pkh') -> do
                      let v       = txOutValue (txOutTxOut o) <> lovelaceValueOf (oFee oracle)
                          p       = assetClassValue (oAsset oracle) $ price (lovelaces $ txOutValue $ txOutTxOut o') x
                          lookups = Constraints.otherScript (swapValidator oracle)                     <>
                                    Constraints.otherScript (oracleValidator oracle)                   <>
                                    Constraints.unspentOutputs (Map.fromList [(oref, o), (oref', o')])
                          tx      = Constraints.mustSpendScriptOutput oref  (Redeemer $ PlutusTx.toData Use) <>
                                    Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toData ())  <>
                                    Constraints.mustPayToOtherScript
                                      (validatorHash $ oracleValidator oracle)
                                      (Datum $ PlutusTx.toData x)
                                      v                                                                      <>
                                    Constraints.mustPayToPubKey pkh' p
                      ledgerTx <- submitTxConstraintsWith @Swapping lookups tx
                      awaitTxConfirmed $ txId ledgerTx
                      logInfo @String $ "made swap with price " ++ show (Value.flattenValue p)
    where
      getPrice :: Integer -> TxOutTx -> Integer
      getPrice x o = price (lovelaces $ txOutValue $ txOutTxOut o) x

      f :: Integer -> Integer -> (TxOutRef, TxOutTx, PubKeyHash) -> Bool
      f amt x (_, o, _) = getPrice x o <= amt

  ```

- Provide Schema with endpoints
  ```haskell
  type SwapSchema =
    BlockchainActions
        .\/ Endpoint "offer"    Integer
        .\/ Endpoint "retrieve" ()
        .\/ Endpoint "use"      ()
        .\/ Endpoint "funds"    ()
  ```
- Combine Contracts with `select`
  - Will offer all endpoints and executes the first which is triggered
  - Loop forever after request returns
  - Funds endoint tells outer world how much funds you own
  - All contracts are wrapped with error handler `h`: Logs error and continues
  ```haskell
  swap :: Oracle -> Contract (Last Value) SwapSchema Text ()
  swap oracle = (offer `select` retrieve `select` use `select` funds) >> swap oracle
    where
      offer :: Contract (Last Value) SwapSchema Text ()
      offer = h $ do
          amt <- endpoint @"offer"
          offerSwap oracle amt

      retrieve :: Contract (Last Value) SwapSchema Text ()
      retrieve = h $ do
          endpoint @"retrieve"
          retrieveSwaps oracle

      use :: Contract (Last Value) SwapSchema Text ()
      use = h $ do
          endpoint @"use"
          useSwap oracle

      funds :: Contract (Last Value) SwapSchema Text ()
      funds = h $ do
          endpoint @"funds"
          v <- ownFunds
          tell $ Last $ Just v

      h :: Contract (Last Value) SwapSchema Text () -> Contract (Last Value) SwapSchema Text ()
      h = handleError logError
  ```


## Funds Module

- In `Oracle.Funds`, two contracts
- `ownFunds` returns all funds
  - Looks up all UTxOs
  - Combines values of all UTxOs
```haskell
ownFunds :: HasBlockchainActions s => Contract w s Text Value
ownFunds = do
    pk    <- ownPubKey
    utxos <- utxoAt $ pubKeyAddress pk
    let v = mconcat $ Map.elems $ txOutValue . txOutTxOut <$> utxos
    logInfo @String $ "own funds: " ++ show (Value.flattenValue v)
    return v
```
- `ownFunds'` variation of `ownFunds` which `tell`s value
  - Writes values into log
```haskell
ownFunds' :: Contract (Last Value) BlockchainActions Text ()
ownFunds' = do
    handleError logError $ ownFunds >>= tell . Last . Just
    void $ Contract.waitNSlots 1
    ownFunds'
```


## Test

- Module `Oracle.Test`
  - Test with `EmulatorTrace`

- Use `runEmulatorTraceIO' def emCfg myTrace`
  - So initial distribution can be set in `emCfg`
    - All wallets get 100 ADA and 100 USD tokens

- Helper contract `checkOracle`
  - Periodically checks value of Oracle
  - Prints log, waits for 1 slot

- Define trace
  - Create params, start Oracle
  - Uses `getOracle` helper function and checks with `observableState` if Oracle is already there
  - Initialize Oracle with `update` endpoint
  - Activate wallets for `ownFunds'` and `swap oracle` contracts
  - Create swaps for wallet 3 and 4 with endpoint `offer`
  - Wallet 5 uses swap with endpoint `use`
    - Note: Not obvious which swap, if swap of wallet 3 or 4
  - Update Oracle to value `1_700_000`
  - Wallet 5 uses swap with endpoint `use`, with new price
  - Update Oracle to value `1_800_000`
    - Note: Wallet 1 can retrieve Oracle usage fees

- Running trace, final balances:
  - Wallet one has roughly 2 ADA more than before: Got Oracle usage fees
  - Wallet 2 has same, nothing done
  - Wallet 3 got USD for rate of 1.7, paid ADA and Oracle usage fees
  - Wallet 4 got USD for rate of 1.8, paid ADA and Oracle usage fees
  - Wallet 5 got offered ADA, payed with USD


## Plutus Application Backend (PAB)

- New: Create executable which runs contract
  - If testnet available: Could be deployed
  - But for now: Use Mockchain

- Create data type for all contracts
```haskell
data OracleContracts = Init | Oracle CurrencySymbol | Swap Oracle.Oracle
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
```
  - `Init` to setup environment (like initial funds in `EmulatorTrace`)
  - `Oracle CurrencySymbol` is `runOracle` contract, which provides `update` to update the Oracle
    - `CurrencySymbol` for communicating currency, USD here
  - `Swap Oracle.Oracle` will run `swap` contract, with all endpoints
  - Required since used in PAB and on frontend

- Create executables in cabal file
  - `oracle-pab`
    - Start simulated wallet
    - Initializes all contracts
    - Starts web server which allows to interact with contracts
  - `oracle-client`
    - Run by Oracle provider
    - Runs Oracle contract
    - Fetches exchange rates from internet and feeds into Oracle
  - `swap-client`
    - Run by clients which want to use swap contract


### Oracle PAB

- Boilerplate code
  - `handleOracleContracts`: Hooking up contracts and schema to become real contracts
  - `handlers`, just copied
  - `initContract` uses `Currency.forgeContract` to add USD tokens to wallets

- Main `main`
  - Uses `Simulator` monad, similar `EmulatorTrace` monad
  - `runSimulationWith`
  - Difference to `EmulatorTrace`: Now IO/side-effects possible
    - Via `MonadIO` and `liftIO`
    - Any `IO` action can be lifted, like
      `liftIO $ writeFile "oracle.cid" ...`

  - Requires to wait for a certain state of a contract
    - Done via helper function `waitForLast`
    - Waits until contract `tell`s a `Last` state (blocks)
    - Note: State is serialized as JSON
      - Must be parsed, can fail
  ```haskell
  waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a
  waitForLast cid =
      flip Simulator.waitForState cid $ \json -> case fromJSON json of
          Success (Last (Just x)) -> Just x
          _                       -> Nothing
  ```
  - E.g. blocks until Oracle NFT was minted
  - Wait for contract to be finished with `Simulator.waitUntilFinished`

  - Start Oracle contract
    - Keep handle for contracts, like `cidOracle`
    - E.g. write into file
    - `cidOracle` is just a UUID

  - Iterate over wallets and activate swap contract
    - Write `cid`s of swap contracts of wallets to files

  - Note: All this can be done from web interface, but easier to do it programatically
    - Check API in `plutus-pab/src/Plutus/PAB/Webserver/API.hs`
    - `NewAPI`, REST API
      - `/api/new/contract`
        - `/activate`
        - `/instances`
        - `/instance/$cid/...`
        - `/definitions`
        - ...
    - Note: Also Websocket API provided

- Execute with `cabal run oracle-pab`
  - Live server starts
  - Check in file explorer: Created `cid` files
  - Required to interact with contract instances


### Oracle Client

- All can be done via REST endpoints and curl, wget, ...
- Here: With Haskell

- Main
  - Reads `oracle.cid`
  - Gets exchange rate from coinmarketcap
  - If exchange rate has changed, update contract
  - Wait 5s, loop

- Updating Oracle in `updateOracle`
  - `POST /api/new/contract/instance/$cid/endpoint/update`
    - New exchange rate as `Integer`, convert to JSON

- Quick and Dirty exchange rate from coinmarketcap `getExchangeRate`
  - Gets homepage
  - Extracts via regex exchange rate

- If updated, logged in PAB


### Swap Client

- Offers via CLI different commands (offer, retrieve, use, funds)
- Uses command line arguments to allow different wallets to use it
  - Reads `cid` from correct file

- Getting funds
  - `POST /api/new/contract/instance/$cid/endpint/funds`
    - `tell`s funds
  - `POST /api/new/contract/instance/$cid/endpint/state`
    - Extract from state the current funds

- Run for wallet 2: `cabal run swap-client -- 2`
- Play around
  - Offer swap in wallet 2: `Offer 1000000`
  - Check funds wallet 2: `Funds`
  - Make swap to wallet 3: `Use`
  - Check funds wallet 3: `Funds`
    - Made swap
    - Payed Oracle fees
    - Payed transaction fees
  - Check funds wallet 2: `Funds`
    - Received USD
    - Live exchange rate was used


## Sum up

- Complete chain
- Not in real world: One PAB for all contracts
  - Required since PAB contains Mockchain
  - In real world: Multiple PABs for different contracts


## Homework

- Try to get demo running
- Play around
- Maybe improve:
  - Different frontends (better UI)
  - Add more Oracles with different sources
  - In swap: Require all three Oracles available, choose value (like average)
