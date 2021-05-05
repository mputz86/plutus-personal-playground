---
date: 2021-04-27T20:37
---

# Cardano Plutus Pioneer Program: Lecture 3

- [Lecture 3 Video](https://www.youtube.com/watch?v=Lk1eIVm_ZTQ)

- Note: Updated plutus repo dependency (newer commit)
  - Code from previous weeks is broken
  - Script address and script context has changed naming

## Recap

- To unlock a script address,
- the script at address is run and
- gets 3 inputs: Data, Redeemer, `ScriptContext`
- Base: All same type, Data
- But preferred: Typed version


## `ScriptContext`

- In package `plutus-ledger-api`, `Plutus.V1.Ledger.Contexts.ScriptContext`
    - Contains: `TxInfo` and `ScriptPurpose`
    - Script Purposes: Spending (most important), Minting, Rewarding, Certifying
    - `TxInfo` describes spending transaction
        - Especially: Inputs, Outputs
        - Stuff for forging, rewards, data
        - `txInfoId` = Hash of transaction with all inputs and outputs
- `txInfoValidRange`: Big advantage over Eth: Transaction already fail in wallet
    - Fails without paying fees if tx invalid because of UTxO already spent
    - Never happen: Script runs and than fails
    - Time must be handled derministically: Added slot range (time interval)
    - Checks before validation:
        - If script runs, time falls in interval of slot range
        - => deterministic
    - By default: Infinity slot range
- [`Plutus.V1.Ledger.Slot`](https://playground.plutus.iohkdev.io/tutorial/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Slot.html)
    - `Slot`
      - Just `Integer` wrapper
      - Derives `Haskell.Num` => simple integer literals can be used
    - `SlotRange`
        - `Interval Slot`
    - [`Interval`](https://playground.plutus.iohkdev.io/tutorial/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Interval.html#t:Interval):
        - Lower bound and upper bound
        - Inclusion/exclusion of bounds
        - Bounds can be infinite
        - Lot of helper functions like
            - `interval` (inclusive range)
            - `singleton`, `to`, `from`, `never`, `always`
        - Useful functions: `member`, `contains`


## Example: Vesting

- Send money to script. Beneficiary address can only access it after some time
- Types:
    - Datum has benificiary and deadline
    - No redeemer
    - Context provides remaining info
- Required checks in validator
    - Benificiary signature is in context
    - Timing, tx executed after deadline
- Validator
    - Check that benificiary is in list of pub key hashes of signatures
    - Only know that time is within slot range (dont know exactly which slot)
        - => All slots in range must be after deadline
- Simulate


## Parametrized Contexts

- Note in simulation
    - If script/smart contract started twice, it has the same script address
    - Because address is computed from validator
        - Validator compiled to Plutus script
        - Hash of Plutus script used as address
    - If the same, same hash, same address

- In last example:
    - Put params in datum (input to validator)
    - Idea: Parametrize script, results in
        - Family of scripts
        - Instantiated with different params
        - => Different scripts
        - => Different addressses

- Building a parametrized validator
    - Add another function parameter with params `p` for validator
        - I.e. neither Datum, Redeemer or Context
    - Datum becomes unit
    - Instantiation
        - `inst` takes param
        - Problem: cannot be used inside Template Haskell `PlutusTx.compile`
            - since it is not statically known at compile time
            - `p` is dynamic value at runtime param
- Solution:
    - Use `PlutusTx.applyCode`
    - With `Lift` class: Allows lifting runtime values to plutus values
        - Use `liftCode`
    - Requires `Lift` instance for `p`
        - `PlutusTx.makeLift`
        - Requires extension: `MultiParamTypeclasses`

- Changes for example:
    - Requires `Slot` in grap
        - In order to construct address that we grab from
            - Requires beneficiary: can wallet find out by itself
            - and slot: Deadlines
        - Reason: Need to pass full params `p` into endpoint
            - Which was provided at instantiation of the contract
- => Now different script addresses


## Lecture 3 Q & A

- Libraries for things like Multi Sig
- Maybe outcome of this pioneer program
- Add something where code of script can be uploaded - since only has stored on blockchain
- Only can look at UTxOs of pub key, no already spent tx



