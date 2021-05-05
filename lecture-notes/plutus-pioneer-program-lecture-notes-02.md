---
date: 2021-04-27T20:39
---

# Cardano Plutus Pioneer Program: Lecture 2

## On-Chain and Off-chain part

- On chain part: About validation
  - Is transaction valid
  - I.e. allowed to consume UTxO
- Off chain part
  - Lives in users wallet
  - Constructs and submits suitable transactions


### On-Chain

- Simple UTxO:
    - Only public key addresses
    - Address given by public key or hash of public key
    - If UTxO sits at such a public key address
    - it can be spent by an transaction if signature belonging to this public key is provided in transaction (constructed with private key)
- For eUTxO:
    - New type of addresses, Script Addresses
        - Arbirarty code
        - When transaction wants to spend a UTxO, node will run script
        - Depending on result of script, transaction is valid or not
    - Instead of signatures, redeemers = pieces of data, provided by tx
    - Datum on UTxO side (arbitrary data, state)
    - Context: transaction to be validated (as input for script)
- => Plutus script 3 data: Datum, Redeemer, Context
    - At lowest level: All 3 same data type `PlutusTx.Data`
    - Has constructors for multiple data (arbitrary data representable)

### Validator

Type: `mkValidator :: Data -> Data -> Data -> ()`
- 3 input data: Datum, Redeemer, Context
- Note: Unusual, since `()` is return type
- No side-effects allowed, beside: throwing exception

- As is:
    - Just haskell function, needs to be compiled to Plutus Core (boilerplate)
    - Done with Template Haskell (`mkValidatorScript` expects `CompiledCode (Data -> Data -> Data -> ())`)
```haskell
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])
```
    - `$$(...)` = splice, run at compile time, put result at this place
    - `[|| ... ||]` = quote, puts source code there - so `compile` can compile it
    - Usually all code needs to be in `[|| ... ||]`, inconvinient (e.g. for helper functions)
    - => Add `{-# INLINABLE mkValidator #-}` pragma to functions in order to put them in quotes

- Address for validator (boilderplate)
```haskell
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = ScriptAddress valHash
```

### Example: Fail and Give-Grab

- Give Endpoint:
    - Creates a tx which pays Ada to script
    - Submits tx, waits for confirmation
    - Prints log

- Grab Endpoint:
    - Gets all UTxO at script address
    - ?? Creates a lookup with UTxOs and script
    - Creates TxConstraints which spends script output
    - All used UTxO must get an input for transaction

- More boilerplate


### Notes

- Uses custom prelude
- Useful for failing:
    - `error :: () -> a`
    - `traceError "NO WAY"`
    - `traceIfFalse`


### Validate only if redeemer fine

- Check 2nd param in `mkValidator`
- Add argument to `grab` endpoint (redeemer)
- Pass argument into `grab` function code



### Example: Typed

- Typed validators, change type to
```haskell
mkValidator :: () -> Integer -> ValidatorContext -> Bool
```
    - Add import `Ledger.Typed.Scripts`
    - Add
```haskell
data Typed
instance Scripts.ScriptType Typed where
    type instance DatumType Typed = ()
    type instance RedeemerType Typed = Integer

inst :: Scripts.ScriptInstances Typed
inst = Scripts.validator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @Integer

validator :: Validator
validator = Scripts.validatorScript inst
```

## Inner Working of conversion form `Data` to `Typed`

- Typeclass `IsData` in `PlutusTx.IsData.Class`
    - Has `toData` and `fromData`
- Existing instances for `()`, `Integer`, ...

- For own types:
    - Write manually instance or
    - Generics: `PlutusTx.unstableMakeIsData ''MySillyRedeemer`
    - Double quotes: Pass type name to function
    - Creates at compile time, `IsData` instance for `MySillyRedeemer`


