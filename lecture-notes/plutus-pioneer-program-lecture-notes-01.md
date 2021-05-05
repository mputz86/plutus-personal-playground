---
date: 2021-04-27T20:39
---

# Cardano Plutus Pioneer Program: Lecture 1

## (E)UTxO Model

### UTxO

- In order to spend
    - Create a transaction which
    - consumes **full** unspent transaction outputs (UTxO) and
    - sends part to other person and
    - remaining ones back to oneself
    - Sum of input must equal sum of outputs
    - Note: Fees must be paid
- Side-effects
    - Wallet is sum of all UTxO which belong to one person
    - If one UTxO does not have enough funds, multiple UTxO must be used as input for Tx
- And: Native tokens can be created but also burned
    - So sometimes input sum does not
- Tx must contain signatures of all input to the Tx

### EUTxO

- Check if UTxO is allowed to be spent by Tx
    - In UTxO-Model: Signature must exist
    - Replaced in EUTxO-Model by arbitrary logic
    - More general addresses, not based on public keys/hashes of public keys
    - (in EUTxO) UTxO contains a `Script` which is executed and checks if this UTxO can be used as input
    - Script expects a `Redeemer`, provided by the Tx, which should provide
    - Important: Context of `Script`, i.e. what data is provided
    - in UTxO-Model: So `Script` used to be the public key and `Redeemer` used to be the signature
    - This is what Bitcoin can do (Bitcoin has smart contracts!)
        - Called Bitcoin-Script
        - Minimal context
    - In Ethereum:
        - Script can see whole state of blockchain (all context)
        - Dangerous to provide all information
        - Too many possibilities of what can happen
    - In Cardano:
        - Can not only see `Redeemer`
        - But complete Tx
    - Final Addition: (arbitrary) Datum
        - In addition to the value (i.e. ADA amount)
    - => Cardano as expressive as Ethereum
    - But: A lot of advances
        - Possible to check if Tx is wallet before sending it to chain
        - Can still go wrong:
            - E.g. if input was already spent when Tx is sent to chain (not preventable)
            - But simply fails, no Tx-fees have to be paid
        - In Ethereum:
            - Unpredictable effects what happens when Tx is executed
            - For example: have to pay Tx fees even so Tx fails
            - => Cardano Scripts easier to verify
            - Cardano better to test since scope is small (compared to full ledger)

- Note: Concept not tied to programming language
    - Any language can be compiled to Plutus core ("assembly language")

- Address belongs to a public key => can be verified by signature


## Demo

- Output can be a script address
- Insights:
  - Code which lives on chain and one that is off-chain
  - Wallets must be able to create Tx for Scripts
  - Code sharing between off- and on-chain part

