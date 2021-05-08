---
date: 2021-05-08T09:38
---

# Cardano Plutus Pioneer Program: Lecture 5

- [Video](https://www.youtube.com/watch?v=6VbhY162GQA)
- Overview:
  - Native tokens, support by Plutus
  - How to define a minting policy
  - Policy: Defines under which conditions native tokens can be minted and burned


## Value

- Value (in Cardano):
  - Each UTxO has address and value
  - In EUTxO each UTxO also has a datum
  - So far: Value was only Ada/Lovelace
  - Exception: Auction example from Lecture 1
    - NFT was auctioned
    - But: Was created at init
    - In Cardano: No tokens other than Ada at beginning

- In `plutus-ledger-api/src/Plutus/V1/Ledger`
  - `Value`
    ```haskell
    newtype Value = Value { getValue :: Map CurrencySymbol (Map TokenName Integer) }
    ```
    - Each token identified by `CurrencySymbol` and `TokenName`
      (both newtype wrapper around `ByteString`)
    - Combination also called `AssetClass`
      ```haskell
      newtype AssetClass = AssetClass { unAssetClass :: (CurrencySymbol, TokenName) }
      ```
    - `Value` states how many units of each asset class contained
  - Special support for `Ada`
    - Represented with `CurrencySymbol` and `TokenName` empty `ByteString`
    - Note: Never construct `Value` directly with `Map`s
    ```haskell
    -- In Plutus.V1.Ledger.Ada
    adaSymbol :: CurrencySymbol
    adaToken :: TokenName
    lovelaceValueOf :: Integer -> Value

    -- Combine `Value`s via Monoid's `<>`
    lovelaceValueOf 123 <> lovelaceValueOf 10

    -- Constructing
    singleton :: CurrencySymbol -> TokenName -> Integer -> Value
    ```

    - `CurrencySymbol` byte string must be valid hexdecimal value (e.g. `a8ff`)
    - `TokenName` can be arbitraty byte string
    ```haskell
    -- Note: Enable OverloadedStrings extension
    singleton "a8ff" "ABC" 7

    -- Example: Combining values
    let v = singleton "a8ff" "ABC" 7 <> lovelaceValueOf 42 <> singleton "a8ff" "XYZ" 100
    v
    > Value (Map [(, Map [("", 42)]), ("a8ff", Map [("ABC", 7), ("XYZ", 100)])])

    -- Extracting
    valueOf :: Value -> CurrencySymbol -> TokenName -> Integer
    valueOf v "a8ff" "XYZ"
    > 100
    valueOf v "a8ff" "abc"
    > 0

    flatten :: Value -> [(CurrencySymbol, TokenName, Integer)]
    ```
- Open Questions:
  - Why two identifiers for asset class
  - Why `CurrencySymbol` must be hexadecimal digitals


## Minting Policy / Monetory Policy

- Transaction can not create or delete tokens
  - All that comes in, goes out
  - Exception: Fees

- `CurrencySymbol` is hash of a script
  - Script is called minting Policy or `MonetaryPolicy`
  - For each transactions that creates or burns a native token
    - `CurrencySymbol` is looked up
    - Script must be contained in transaction
    - Script is executed
    - Along with other validation scripts
    - Script decides if transaction is allowed to mint or burn tokens
  - Ada fits as well:
    - Hash is empty
    - No script exists, so
    - No way to mint or burn Ada
    - => All come from genesis transaction
    - => Amount of Ada is fixed, does never change
  - Only custom native tokens can have minting policy

- Recap: Validation
  - If no pubkey address, but script address
  - And UTxO which sits at this script address
  - Have tx which tries to consume this UTxO as input
  - For each script input corresponding validation script is run
  - Each validation script becomes
    - Datum from UTxO
    - Redeemer from input
    - Context

- Data types
  ```haskell
  data ScriptContext = ScriptContext { scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }

  data ScriptPurpose =
    = Minting CurrencySymbol
    | Spending TxOutRef
    | Rewarding StakingCredential
    | Certifying DCert

  data TxInfo = TxInfo
    { ...
    , txInfoForge   :: Value
    ...
    }
  ```
  - Minting policy triggered if `txInfoForge` non-empty
    - I.e. for each value, script is run (at address belonging to `CurrencySymbol`)
    - Minting policies only get one input: Context
      - Same as before
      - No datum: Since it belongs to an UTxO
      - No redeemer: Since it belongs to an input
      - Forging belongs to transaction itself, no specific input or output
  - So far only `Spending` as `ScriptPurpose` in `ScriptContext` used
    - In case of forging/minting purpose is: `Minting CurrencySymbol`
      - Note: `ownCurrencySymbol` uses the purpose to determine the scripts own currency symbol
    - If in `txInfoForge` multiple different `CurrencySymbol`s,
    - the corresponding minting policies will be called with their `ScriptPurpose`

- Example of Minting Policy
  - Arbitrary forging and minting (by anybody) of this currency
```haskell
{-# INLINEABLE mkPolicy #-}
mkPolicy :: ScriptContext -> Bool
mkPolicy _ = True

-- Compile to Plutus script
policy :: Scripts.MonetoryPolicy
policy = mkMonetaryPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolic mkPolicy ||])

-- Compute `CurrencySymbol` of policy
curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy
```

- Off-chain part
  - Allows creation and destruction of coins
  - Missing: `TokenName`
```haskell
data MintParams = MintParams
  { mpTokenName :: !TokenName
  , mpAmount    :: !Integer
  }

type FreeSchema =
  BlockchainActions
    .\/ Endpoint "mint" MintParams
```

- `Contract` Function
```haskell
mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
  let val     = Value.singleton curSymbol (mpTokenName mp) (mpAmount mp)
      lookups = Constraints.monetaryPolicy policy
      tx      = Constraints.mustForgeValue val
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ awaitTxConfirmed $ txId ledgerTx
  Contract.logInfo @String $ printf "forged %s" (show val)
```

  - `mint`'s `Contract` return type:
    - `w` is tell/writer: Not used here, so not specified
    - `FreeSchema`: `BlockchainAction`s plus access to self-defined mint-endpoint
    - `Text`: Type of error messages
    - `()`: Returns nothing
  - In order to create a transaction `tx`
    - Not "obvious" approach taken with specifying each field on your own
      - Since a lot of repetition, therefore tedious.
      - Required to compute fees, ...
    - Approach taken: Define constraints
      - Declaratively define conditions which transaction must fulfill
      - Like `mustForgeValue`, `mustPayToPublicKey`, ...
      - Let library resolve constraints to a valid transaction
    - Resolving for example in our case:
      - Balances transaction: E.g. add input which cover transaction fees
      - Newly forged coins must be sent somewhere: Sent to own wallet
      - If burned: Coins must come from somewhere (like, from own wallet)
    - Construct transaction and sent it to chain via `submitTxConstraintsWith`
      - May fail, e.g. if not enough funds
    - Lookups
      - Required to fulfill contraints and construct transaction
      - Additional information required
      - E.g. in order to run the policy script,
        - the script must be included: `lookups = Constraints.monetaryPolicy policy`
        - since `Value` of `txInfoForge` only has hash of policy script (`CurrencySymbol`); i.e. not script itself
        - Required for nodes to validate transaction
        - Constraint `mustForgeValue` "requests" the policy script
      - Lookups kind of way to hint where to find information
        - Other lookups: UTxOs, validation scripts
    - Note: The `@Void` required since `submitTxConstraintsWith` does not use a `Datum` or `Redeemer` and therefore type is not known
  - `awaitTxConfirmed`: Wait for transaction to confirm
    - Note: If transaction fails to validate, this line blocks forever
    - Soon: Probably API changed to provide function to listen to status changes and handle them

- Endpoints and remaining code
  ```haskell
  endpoints :: Contract () FreeSchema Text ()
  endpoints = mint' >> endpoints
    where
      mint' = endpoint @"mint" >>= mint
  ```
  - Call `mint'` and recursively call `endpoints` again, run forever
  - `MintParams` of `mint` function received via endpoint
  - Contract action `endpoint @"mint"` blocks until endpoint called / `MintParams` are provided

## Realistic Example

- Restrict minting and burning by transactions signed by specified pub key hash
  - Like central bank
  - Parametrize monetary policy with public key hash
  - Check in validator if transaction is signed by this public key hash
  - Note: Requires to make adaptions for parametrized validator everywhere
  - In `mint` Contract:
    - Get own public key
    - Parametrize script with it
```haskell
{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> ScriptContext -> Bool
mkPolicy pkh ctx = txSignedBy (scriptContextTxInfo ctx) pkh

policy :: PubKeyHash -> Scripts.MonetaryPolicy
policy pkh = mkMonetaryPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh

curSymbol :: PubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

-- ...
mint :: MintParams -> Contract w SignedSchema Text ()
mint mp = do
  pkh <- pubKeyHash <$> Contract.ownPubKey
  let val     = Value.singleton (curSymbol pkh) (mpTokenName mp) (mpAmount mp)
  -- ...
```

## Non-fungible Tokens (NFTs)

- Only one coin in existence
- Option 1: Naive idea:
  - Check in validation that only one coin is foged in `txInfoForge`
  - But then: Restricts only to one coin per one transaction
- Option 2: Use Deadlines
  - Like done on Cardano since Mary
  - Only before this time, minting is allowed
  - I.e.: Current "NFT"s on Cardano not really NFTs, need to look on blockchain if really just one token exists
- Option 3: Real NTFs with Plutus
  - Must prevent more than one minting transaction
  - Policy must only a single transaction to do it
  - Seems impossible
    - Can do transaction again
    - Even with deadlines: Can do transaction multiple times in same slot
  - Need something unique
    - Something that exists only in one transaction, never again
    - Trick: Use an UTxO
      - Can only exist once
      - If consumed, no longer there
      - Never ever be same transaction again
    - UTxO = output of a transaction
      - Transaction ID + Index of output of transaction
      - Note: Transaction outputs are ordered
    - Transactions are unique as well
      - Subtle: Transaction only unique because of fees
      - E.g. transaction without inputs and only outputs without value
        - These could exist multiple times: Exact same hash and exact same transaction id
        - But: Not possible in Cardano since
          - fees require at least one input which pays for fees
          - and the fee must come from an UTxO
          - And: No double spending possible
  - In essence:
    - Specify UTxO as param for minting policy and
    - spent exactly this UTxO, check for spending during validation

- Type of UTxO is `TxOutRef`
  - Found in `TxInInfo` (from `ScriptContext`, `TxInfo`)

- NFT Policy
  - Parametrize policy on UTxO, `TxOutRef`
  - Check if transaction spents the specified UTxO
  - Check if the forged amount is 1
  - Chicken-egg-problem with `CurrencySymbol`:
    - Is computed from the policy
    - But required in order to check for it
    - Use `ownCurrencySymbol :: ScriptContext -> CurrencySymbol`
  - Note: Two params, requires twice `PlutusTx.applyCode`
  - Endpoint just requires the `TokenName`
```haskell
{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> ScriptContext -> Bool
mkPolicy oref tn ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                       traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoForge info) of
        [(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && tn' == tn && amt == 1
        _                -> False

policy :: TxOutRef -> TokenName -> Scripts.MonetaryPolicy
policy oref tn = mkMonetaryPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMonetaryPolicy $ mkPolicy oref' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn
```
  - UTxO must be looked up by wallet creating the contract
    - Since a wallet can only spent it's own UTxOs
    - Done via `utxoAt` with own address, via `pubKeyAddress :: PubKey -> Address`
    - Returns an `AddressMap.UtxoMap` (maps `TxOutRef` to `TxOutTx`)
    - Pick one of the keys as input (does not matter which one)
  - Add constraint that requires to spent the UTxO with `mustSpentPubKeyOutput`
    - Constraints form `Semigroup`: Combine with `<>`
    - Required to add lookup of UTxOs to resolve transaction constraints via `unspentOutputs :: UtxoMap -> ScriptLookups a`

```haskell
mint :: TokenName -> Contract w NFTSchema Text ()
mint tn = do
    pk    <- Contract.ownPubKey
    utxos <- utxoAt (pubKeyAddress pk)
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let val     = Value.singleton (curSymbol oref tn) tn 1
                lookups = Constraints.monetaryPolicy (policy oref tn) <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustForgeValue val <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)
```


## Homework

1. Modify `Signed` policy with second parameter `Slot` for a deadline
2. Simplify `NFT` policy Remove `TokenName`, use empty `ByteString`



