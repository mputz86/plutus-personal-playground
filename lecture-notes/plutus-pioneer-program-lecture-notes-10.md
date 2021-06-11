---
date: 2021-06-05T10:11
---

# Cardano Plutus Pioneer Program: Lecture 10

- Walkthrough of Uniswap clone in Plutus
  - From Ethereum
- PAB requests via `curl`

## Uniswap

- Decentralized Finance (DeFi) application
- Allows swapping of tokens without central authority
  - I.e. replaces centralized exchange
  - In Ethereum: Tokens are ERC-20
- Does not use an order book (usually used for price discovery)
  - Uses automatic price discovery system
- Idea: People can create liquidity pools
  - Exchange of two tokens
  - Creator of liquidity pool just adds amount of both pools into this pool
  - Creator will get liquidity tokens in return
  - Liquidity tokens are specific to this pool
- Users can
  - Take one amount of tokens out, in exchange for the other token
  - Add liguidity to pool, i.e. amounts of both tokens
  - Remove liquidity from pool by getting tokens out in exchange for liquidity tokens


## Setup

- Initial: Factory with a NFT and list of all liquidity pools (initially `[]`)
  - Ensures that only one pool for each possible pair exists
- In order to add liquidity, Alice adds tokens from A and B
  - Note: Ratio between A and B reflects Alice's believe in relative value of tokens
- Create liquidity pool:
  - Provide tokens A and B
  - Creates a PoolAB, contains
    - As and Bs
    - AB NFT
    - Amount of liquidity tokens Alice received, AB
  - Uniswap Factory
  - Alices receives liquidity tokens
    - Here: 1415 (square root of 1000*2000)

- Pool can then be used for swapping
  - Bob provides 100A, receives 181 B
  - Pool AB contains 1100 A, 1819 B, AB NFT, 1415
- How 181 B computed?
  - Idea about price discovery
  - Rule: Product of amounts of tokens must never decrease
- So should keep ration 1:2 up
  - But: The more amount you take out, the more expensive it gets
  - Results in next user would get less than 181 Bs for 100 As
  - In essence: Adapts to demand
- On top: Fees for pool creator
- Since product then not only stays but increases
  - Alice can burn all liquidity tokens and has
  - Gains

- Adding liquidity:
  - Add tokens of believe of correct ration (by Charlie)
  - Gets back newly forged liquidity tokens
  - Formula a bit more complicated
    - In essence: Also works with product
    - Check product before and after addition
    - And take into account how many liquidity tokens minted so far
  - In sum: Received liquidity tokens are fair according to contribution

- Remove: Allows burning of liquidity tokens
  - Gets tokens of current ration in pool
  - I.e. removing does not change the ration

- Close: Only possible if remaining liquidity tokens are burned
  - Requires factory as input
  - Note: Factory only used when list of pools is modified
    - So congestion is very low


## Code

- Part of `plutus-use-cases`: `Plutus.Contracts.Uniswap`
  - `OnChain`, `OffChain`
  - `Types`: Common types of other code
  - `Pool`: Business logic, formulas

### `Types`

- `U`: Uniswap coin, identifies factory
- `A`, `B`: Used for pool operations
- `PoolState`: Token which identifies pool
  - Does not need to be an NFT
- `Liquidity`: Liquidity tokens

- `Coin a`: Uses all other types
  - `a` is a phantom type
    - No representation at run-time
    - Used to not mix up various coins

- `Amount a`:
  - Wrapper around `Integer`
  - Avoids mixing up amounts

- `valueOf :: Coin a -> Amount a -> Value`
  - See phantom type: Cannot mix up amount of coin A with amount of coin B

- `Uniswap`: Identifies instance of running Uniswap system
  - Note: Nobody can be prevented from setting up own factory
  - Is wrapper around coint

- `LiquidityPool`
  - Contains both coins
  - Note: Order should not matter, so specific `Eq` instance

- `UniswapAction`
```haskell
data UniswapAction
  = Create LiquidityPool
  | Close
  | Swap
  | Remove
  | Add
```

- `UniswapDatum`
```haskell
data UniswapDatum
  -- Used by factory
  = Factory [LiquidityPool]
  -- Used by pools
  | Pool LiquidityPool (Amount Liquidity)
```

### `Pool`

```haskell
-- Create a new pool with the given ration
calculateInitialLiquidity :: Amount A -> Amount B -> Amount Liquidity

-- Adding liquidity to an existing pool
-- - First two arguments: Current tokens in pool
-- - Already minted liquidity tokens
-- - 4th and 5th argument: To add tokens
-- - Return allowed amount of liquidity tokens
calculateAdditionalLiquidity
  :: Amount A
  -> Amount B
  -> Amount Liquidity
  -> Amount A
  -> Amount B
  -> Amount Liquidity

-- Returns how many tokens remain in pool
-- Provide existing amount of A and B plus current liquidity
-- 4th: Liquidity tokens wanted to exchange
calculateRemoval
  :: Amount A
  -> Amount B
  -> Amount Liquidity
  -> Amount Liquidity
  -> (Amount A, Amount B)

-- Calculate swap:
-- - First two, current situation
-- - 3rd and 4th: Remaining tokens in pool
-- - Ensures that amount a or B never gets 0 (cannot increase anymore)
checkSwap :: Amount A -> Amount B -> Amount A -> Amount B -> Bool

-- Compute name based on token
-- Note: Ensured that order does not matter
ipTicker :: LiquidityPool -> TokenName
```

### `OnChain`

- `mkUniswapValidator`: Create validator
  - For both, factory and pool
  - I.e. share same script address
    - Only distinguised by datum and
    - by coins that identify them
  - Handles all possible cases
  - Note: Code quite long, but all explained in example
- `validateLiquidityForging`: Monetory policy script for liquidity tokens
  - Does not contain any logic
  - But delegates to uniswap validator
    - Looks at inputs, checks if factory or pool

- Note: Not done via state machines
  - Question if even possible; at least it is not obvious
  - Anyway: Factory and pools behave like state machines
  - Since sometimes both, factory and pool, is involed in transition, unclear how to represent in a state machine


### `OffChain`

- Two schemas
  - `UniswapOwnerSchema`: For creator of uniswap factory
    - Only `start` endpoint
  - `UniswapUserSchema`
    - All possible user interactions
    - Each action parameterized by factory of first call

- Use writer monad's `tell` mechanism
  - `UserContractState` contains state for all user contract states

```haskell
data UserContractState =
      Pools [((Coin A, Amount A), (Coin B, Amount B))] -- ^ Query all existing pools
    | Funds Value                                      -- ^ Existing funds in a wallet
    | Created                                          -- ^ All other operations, when happened
    | Swapped
    | Added
    | Removed
    | Closed
    | Stopped
    deriving (Show, Generic, FromJSON, ToJSON)
```

- Parameters for endpoints
```haskell
-- For Creation: Need to know tokens and the amounts (ration)
data CreateParams = CreateParams
    { cpCoinA   :: Coin A
    , cpCoinB   :: Coin B
    , cpAmountA :: Amount A
    , cpAmountB :: Amount B
    } deriving -- ...

-- For Swap: Must know tokens and amout for one of the coins. Other amount is 0.
data SwapParams = SwapParams
    { spCoinA   :: Coin A
    , spCoinB   :: Coin B
    , spAmountA :: Amount A
    , spAmountB :: Amount B
    } deriving -- ...

-- For Close: Just which pool is required, i.e. identified by two tokens.
data CloseParams = CloseParams
    { clpCoinA :: Coin A
    , clpCoinB :: Coin B
    } deriving -- ...

-- For Remove: Specify pool and how much liquidity to burn.
data RemoveParams = RemoveParams
    { rpCoinA :: Coin A
    , rpCoinB :: Coin B
    , rpDiff  :: Amount Liquidity
    } deriving -- ...

-- For Add: Identify pool and how many As and Bs to add.
data AddParams = AddParams
    { apCoinA   :: Coin A
    , apCoinB   :: Coin B
    , apAmountA :: Amount A
    , apAmountB :: Amount B
    } deriving -- ...
```

- Contrant implementation
  - `start` forges the token for the factory
    - Result is `Uniswap`, i.e. factory which needs to be provided for to other contracts
  - `create`, `close`, `remove`, `add`, `swap`
    - All access business logic in `Pools` module
    - I.e. buisness logic used in on-chain and off-chain code
  - `pools`: Queries exiting pools
  - `funds`: Checks own funds in wallet and returns them
  - All: Return result

- Endpoints: `ownerEndpoint`, `userEndpoints` (contains all user actions)

- Idea to write result into state
  - Done in endpoint definitions
  - Uses `Last` monoid
  - Allows error by using `Either Text a`
  - E.g. `userEndpoints :: Uniswap -> Contract (Last (Either Text UserContractState)) UniswapUserSchema Void ()`

- Also: Added `stop` endpoint to `userEndpoints` to allow stopping of contract


## PAB

- Front available in `plutus-pab/examples/uniswap`
  - Copied and modified `Main.hs` from there for this lecture

- Cabal file: Two executables
  - `uniswap-pab`: Server
  - `uniswap-client`: Console based frontend
  - Both use `Uniswap` (in `other-modules`), contains common definitions

- Common `Uniswap` module
  - `UniswapContract`: Captures various instances which can be run for wallets
    - `Init`: Create and distribute example tokens
    - `UniswapStart`
    - `UniswapUser`: Other user endpoints, interact with uniswap
      - Parameterized by `Uniswap.Uniswap`, a uniswap instance
      - Result of `UniswapStart`

- `Init`
  - Creates 1000000 tokens of A, B, C, D for each wallet
  - Distributes them
  - For contract instance id communication, write files (`*.cid`)

- `uniswap-pab`
  - `handleUniswapContract`: Already shown boilerplate for hooking up PAB mechanism with actual contracts
  - `main`
```haskell
main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    -- Run in 'Simulator' monad
    logString @(Builtin UniswapContracts) "Starting Uniswap PAB webserver on port 8080. Press enter to exit."
    -- Start server and get handle on it for shutting it down
    shutdown <- PAB.Server.startServerDebug

    -- Mint and distribute tokens
    cidInit  <- Simulator.activateContract (Wallet 1) Init
    -- Wait until init returns with currency symbol, 'cs'
    cs       <- flip Simulator.waitForState cidInit $ \json -> case fromJSON json of
                    Success (Just (Semigroup.Last cur)) -> Just $ Currency.currencySymbol cur
                    _                                   -> Nothing
    _        <- Simulator.waitUntilFinished cidInit

    -- Write currency symbol into file
    liftIO $ LB.writeFile "symbol.json" $ encode cs

    logString @(Builtin UniswapContracts) $ "Initialization finished. Minted: " ++ show cs

    -- Start uniswap system from wallet 1
    cidStart <- Simulator.activateContract (Wallet 1) UniswapStart
    -- Wait until start has finished with returning a Uniswap - required for all user endpoints
    us       <- flip Simulator.waitForState cidStart $ \json -> case (fromJSON json :: Result (Monoid.Last (Either Text Uniswap.Uniswap))) of
                    Success (Monoid.Last (Just (Right us))) -> Just us
                    _                                       -> Nothing
    logString @(Builtin UniswapContracts) $ "Uniswap instance created: " ++ show us

    -- Activate user instances for all wallets
    forM_ wallets $ \w -> do
        -- Activate: Returns handle for interaction with frontend
        cid <- Simulator.activateContract w $ UniswapUser us
        -- Write handles into files
        liftIO $ writeFile (cidFile w) $ show $ unContractInstanceId cid
        logString @(Builtin UniswapContracts) $ "Uniswap user contract started for " ++ show w

    -- Wait for user to press any key before shutting down server
    void $ liftIO getLine
    shutdown
```

  - Running will mint tokens, create files etc.
    - Files
```bash
> ls -1
add.sh
app
cabal.project
...
symbol.json
W1.cid
W2.cid
W3.cid
W4.cid
> cat symbol.json
{"unCurrencySymbol":"8a316f6dbbfc070ce13724269c2e10cfec367f6059af8316d84f7196f18d9346"}
> cat W1.cid
1f71f03b-e071-42a9-8f42-9ee4824c5871
```

- `uniswap-client`
  - Expects one command line parameter: 1, 2, 3 or 4 (wallet number)
  - Reads currency symbol
  - Loops over commands and executes them
    - Uses `Char` for identifying token (`A`, `B`, `C` or `D`)
  - Command helper functions
    - `callEndpoint` for creating requests
      - `callEndpoint :: ToJSON a => UUID -> String -> a -> IO ()`
      - Response always `()`
      - So just checks if status code is `200`
    - Uses `getStatus` for getting status
      - No request body
      - Need to tell what expected result is, `ContractInstanceClientState UniswapContract`
      - Check state of result, `fromJSON $ observableState $ cicCurrentState $ responseBody w`
  - With these two helper functions, all other easy to implement
    - E.g. `getFunds`
```haskell
getFunds :: UUID -> IO ()
getFunds cid = do
    callEndpoint cid "funds" ()
    threadDelay 2_000_000
    go
  where
    go = do
        e <- getStatus cid
        case e of
            Right (US.Funds v) -> showFunds v
            _                  -> go

    showFunds :: Value -> IO ()
    showFunds v = do
        showCoinHeader
        forM_ (flattenValue v) $ \(cs, tn, amt) -> showCoin cs tn amt
        printf "\n"
```
    - `createPool`
```haskell
createPool :: UUID -> US.CreateParams -> IO ()
createPool cid cp = do
    callEndpoint cid "create" cp
    threadDelay 2_000_000
    go
  where
    go = do
        e <- getStatus cid
        case e of
            Right US.Created -> putStrLn "created"
            Left err'        -> putStrLn $ "error: " ++ show err'
            _                -> go
```

- Running with
  - Run clients, for each client
  - E.g for wallet 1 `cabal run uniswap-client -- 1`
    - Use commands `Funds`, `Pools`

## Executing

- Play through example from beginning
  - Alice (wallet 1): Create a pool `Create 1000 'A' 2000 'B'`
    - Query `Pools`
  - Bob (wallet 2): Swap `Swap 100 'A' 'B'`
  - Charlie (wallet 3): Adds liquidity `Add 400 'A' 800 'B'`
  - Alice: Remove her liquidity `Remove 1415 'A' 'B'`
  - Charlie: Closes pool `Close`

- With curl
  - Simple get requests
```bash
> cat status.sh
#!/bin/sh
curl "http://localhost:8080/api/new/contract/instance/$(cat W$1.cid)/status" | jq ".cicCurrentState.observableState"

> cat funds.sh
#!/bin/sh
curl  "http://localhost:8080/api/new/contract/instance/$(cat W$1.cid)/endpoint/funds" \
    --header 'Content-Type: application/json' \
    --data-raw '[]'
```
  - Post requests for creation
```bash
> cat create.sh
#!/bin/sh

symbol=$( cat symbol.json )
body="{\"cpAmountA\":$2,\"cpAmountB\":$4,\"cpCoinB\":{\"unAssetClass\":[$symbol,{\"unTokenName\":\"$5\"}]},\"cpCoinA\":{\"unAssetClass\":[$symbol,{\"unTokenName\":\"$3\"}]}}"
echo $body

curl  "http://localhost:8080/api/new/contract/instance/$(cat W$1.cid)/endpoint/create" \
--header 'Content-Type: application/json' \
--data-raw $body


> ./create.sh 1 1000 A 2000 B
> ./status.sh 1
...
# Repeat until state is updated
```
    - How to get request body (`body`)?
      - Is written by CLI, in `uniswap-client.hs`
      - In `callEndpoint`: `liftIO $ printf "request body: %s\n\n" $ B8.unpack $ encode a`
      - Tip: Use it in repl to find out body
