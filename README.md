
# Personal Playground

Playground for my own personal experiments.

Note: Heavily relies on source code in/from
- [plutus](https://github.com/input-output-hk/plutus) repository
- Especially the [use-cases](https://github.com/input-output-hk/plutus/tree/master/plutus-use-cases) project
- Some code from [plutus-pioneer-program](https://github.com/input-output-hk/plutus-pioneer-program) was copied for easy reference (in `src/WeekXX` folders like in original repo)


## Setup `nix-shell` Env

### Simple

Naively, call `nix-shell` with path to `shell.nix` in `plutus` repo.
```bash
plutus-pioneer-program/code/week08 $ nix-shell ../../plutus/shell.nix
```

### Advanced: Automatic loading on entering project directory

Advantages:
- Automatically sets up `nix-shell` when you enter any project directory
  - Especially whenever you enter a sub-directory
  - Like `cd plutus-pioneer-program/code/week08`
- Preserves your shell (like `zsh`)
  - Instead of `nix-shell`'s default shell, `bash`
- Note (maybe good or bad):
  - Installs pre-commit hooks
  - Like formatting nix files

Setup:
- Add `shell.nix` to project dir (like `plutus-pioneer-program`)
  ```nix
  { }:
  let
    # Import shell from plutus repo
    # Makes tools like haskell-language-server available
    shell = import ./../plutus/shell.nix {};
  in
    shell
  ```
  - Assumes `plutus` repo is on same level, like
    ```bash
    > tree -L 1
    .
    ├── plutus
    ├── plutus-pioneer-program
    ├── plutus-personal-playground
    ```
- Load `nix-shell` automatically with `direnv`
  - Install [`direnv`](https://github.com/direnv/direnv)
  - Install [`nix-direnv`](https://github.com/nix-community/nix-direnv)
    - Probably the most complicated step
  - Automatically load env by adding `use nix` to `.envrc` in project folder
    ```bash
    > cd plutus-pioneer-program
    > echo "use nix" >> .envrc
    > direnv allow
    ```
  - Test by exiting, re-entering and trying to use `haskell-language-server`
    ```bash
    > cd
    > cd -
    > haskell-language-server --version
    haskell-language-server version: 1.1.0.0 (GHC: 8.10.4.20210212) (PATH: /nix/store/ygfxr40id5jsyg2g3yb0drqj0fspp322-haskell-language-server-exe-haskell-language-server-1.1.0.0/bin/haskell-language-server)
    ```

---


## Test Cases


### Test Case: Parallel Auction

Source code: [src/ParallelAuction.hs](src/ParallelAuction.hs).

**[Details and Learnings](docs/LearningsParallelAuction.md)**.



### Await Tx Confirmed

Source code: [src/Issues/AwaitTxConfirmedIssue.hs](src/Issues/AwaitTxConfirmedIssue.hs).

Issue brought up by @dino in discord.
- `awaitTxConfirmed` loops forever if validation fails
- I.e. contract is blocked for further requests
- Same as what happened in lecture 02

Solution:
- Add a timeout to the `awaitTxConfirmed`

- Added helper functions for
  - Waiting for timeout (in `Slot`s)
  - And either throws and error, `withTimeoutThrowError`, or
  - just logs the timeout, `withTimeoutLogging`.
  - Usage example:
```haskell
mint :: Integer -> Contract w SignedSchema Text ()
mint amt = do
  let val = Value.singleton curSymbol "ABC" amt
      lookups = Constraints.monetaryPolicy policy
      tx = Constraints.mustForgeValue val
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
  withTimeoutLogging 2 $ do
    Contract.logInfo @String $ printf "Awaiting confirmation"
    awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)
```

- Note: Transaction confirmation handling will change in future, so probably not worth to contribute




### Tx Input Datum in Validator / On Chain

Source code: [src/InputTxDatumsIssue.hs](src/InputTxDatumsIssue.hs).

Assumed that a validator has access to `Datum` of the UTxOs spent by the transaction. Since there is a `DatumHash` given, it is `Just ...`. But the hash can nowhere used.

Solution: `mustIncludeDatum` actually solves the issue. So in essence it just adds the `Datum` with the hash to `txInfoData` and then I can pick them up with `findDatum` :tada:

Summing it up:
- `Datum` of outputs seem to be automatically added to the `txInfoData` in the `ScriptContext`
- `Datum` of inputs are only references via their `DatumHash` but not availabe in `txInfoData` per default.
- Any `Datum` can be added via `mustIncludeDatum` to `txInfoData` and be found via `foundDatum`
- I.e. also the missing `Datum` of the inputs


Official:
- `Datum`s of inputs must be automatically available
- Added [issue](https://github.com/input-output-hk/plutus/issues/3119), which is already solved



### Test Case: Payback

See [src/PayBackIssue.hs](src/PayBackIssue.hs) for the implementation.

Somehow paying back an UTxO with a token attached resulted in the transaction submitter "payed the payback". Problem was the missing split of the value of the UTxO into the native (ADA) amount and the token before using the value.
