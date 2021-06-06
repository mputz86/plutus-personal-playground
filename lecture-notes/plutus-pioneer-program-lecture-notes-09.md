---
date: 2021-06-05T10:11
---

# Cardano Plutus Pioneer Program: Lecture 9

- Recap
  - So far: All important ingredients for writing Plutus application:
    - eUTxO, On-Chain validation, minting policies, off-chain code, deployment and testing
  - Plutus allows writing interpreter for other languages
  - One of them: Marlowe
    - Domain specific language (DSL) for financial contracts


## Marlowe by Prof. Simon Thompson

- Marlowe:
  - Special-purpose language for financial contracts
- Reason for special purpose languages?
  - Language which is close to language of the user
    - And not of the system
  - I.e. in the domain of the specific application
    - Like financial, payment
  - Cant write as much as in a general purpose language
  - Working in more specialized context, allows to
    - Better feedback
    - Better error messages
    - More guarantees on program behaviours (focus in this lecture)

### Assurances by DSL

- Contracts do what they should and
  - not what they shouldn't
- Implementation as simple as it can be
- Contracts can be read an simulated
  - Allows to view *all** behaviour of the contract
  - I.e. every possible execution path
- System can be proofed to be safe
  - I.e. by writing mathematical proofs


### Financial contract

- Definition
  - Accept payments from and to participants
  - Can evolve in different directions
    - Dependent e.g. on choices of participants
    - or external sources like stoke exchanges (i.e. Oracles)
  - Participants can have roles
    - Roles can be owned, i.e. owning token is evidence of having role
    - Represented by minted tokens
    - Also means: Roles become tradeable

### Design of Language

- Contract is just a program running on a blockchain
- Features
  - Can run forever
  - Could wait forever for an input (like a choice)
  - Terminate holding assets forever
  - "Double spent" assets
- I.e. security issues a contract may have

- Marlowe: Design for safety
  - Contracts are finite:
    - No recursion,
    - no loops
  - Contracts will terminate
    - By timeouts on all external actins
    - Like choices, deposits
    - I.e. can not wait forever, alternative choice taken when timeout occurs
  - Defined lifetime
    - Read off from timeouts
  - No assets retained on close
    - (Local) accounts get refunded on close
    - At end of lifetime
  - Conservatin of value
    - Guaranteed by underlying blockchain
    - Like not allowing double spending

- Note: Guarantees are not given by Plutus applications

- Definition
```haskell
data Contract
  = Close
  | Pay Party Payee Value Contract
  | If Observation Contract Contract
  | When [Case Action Contract]
      Timeout Contract
  | Let ValueId Value Contract
  | Assert Observation Contract
```
  - `Pay`: On `Party` of the contract pays to `Payee` a `Value` and then `Contract` continues (= continuation contract)
  - `If`: Chooses first `Contract` if `Observation` is true, otherwise second `Contract`
  - `When` (most complex):
    - List of possible `Action`s
    - `When` waits for the set of possible `Actions`
      - E.g. for a deposit
    - Whenever an action happens, it executes the corresponding `Contract`
    - To avoid waiting forever:
      - Whenever the `Timeout` occurs
      - Execute the alterntive, the last `Contract`
  - `Close`: Finish the contract


### The Marlowe Product

- Marlowe Suite contains
  - Run
    - End user can interact with smart contracts on Cardano blockchain
    - Obtain and run contracts
    - State: Prototype exists, demo shown
    - Runs in browser
      - Currently: Blockchain simulated in browser
  - Market
    - Contracts can be uploaded and downloaded
    - Gives some assurances
    - State: Not available?
  - Play
    - Interactive simulation of contracts
  - Build
    - Contracts built in code, visually or embedded
  - State of Play & Build
    - Currently combined in Marlowe Playground

- Aim:
  - Everything up and running when Plutus PAB etc. is available on Cardano mainnet
  - Now: Local
  - In few weeks: Distributed on testnet
  - End of year: On mainnet

- Demo of Marlowe Run https://staging.marlowe-dash.iohkdev.io/



### Engineering

- Executing a Marlowe contract
  - Executing produces a series of transactions
  - Flow:
    - Marlowe Run (off-chain part)
      - Builts the valid transactions
      - Submits transactions to blockchain
      - Singing of transaction via connection to wallet (like Deadalus)
    - On-chain: Checked by Marlowe interpreter (= Plutus contract; very large contract)
    - Flow back from on-chain to off-chain: About updates in the contract (like deposit was made), via companion contract



### System Design

- Semantics: Is a Haskell function
  ```haskell
  reduceContractStep :: Environment -> State -> Contract -> ReduceStepResult
  reduceContractStep env state contract = case contract of
    Close -> ...
    Pay ... -> ...
  ```
- => Semantics is executable specification in Haskell
  - Denotation semantics defined through an interpreter
  - Completness: All cases must be covered
  - Engagement: Semantics can be run

- Purpose of semantics: Reusable
  - In Isabelle: For reasoning and proof
  - In Plutus: For implementation on blockchain
  - In PureScript: For browser-based simulation
- How to be sure that Isabelle and PureScript semantics are the same (as Haskell)?
  - Extract Haskell from Isabelle
  - Test extracted version to original Haskell version (on random contracts)
  - Eventually replace PureScript with a Haskell-to-JS implementation



### Usability

- Allowing different ways of writing contracts
  - Haskell (e.g. in Marlowe Playground)
  - Visual editor
  - Embedded DSL, for Haskell and JS
  - Generate contracts (based on contract terms)

- Allowing to explore interactively how contract behaves (before running in a simulation)
  - Simulate with [Playground](https://play.marlowe-finance.io/)
  - Allows to undo last step, try another path, ...
  - Note: Changing UI/UX to see series of steps like in Marlowe Run


### Assurance

- Power of Logic
  - Static analysis: Guarantee that contract does what it should be checking each possible path
  - Verification: Machine-supported proof of system and contract properties

- Static Analysis
  - Check all execution paths
  - All choices, e.g. also all possible slots for a transaction submit
  - E.g. possible that a `Pay` fails?
    - If so, give a counter example
    - Static analysis can be preformed in playground

- Verification
  - Prove properties of Marlowe system once and for all
  - E.g.
    - Theorem: Accounts are never are negative
    - Theorem: all_money_in = money_in_accounts + money_out (money preservation)
    - Theorem: Close produces no warning
    - Theorem: Static analysis is sound and complete


### Resources

- Marlowe and Plutus repositories
- IOHK research library
- Online tutorial for Marlowe Playground




## Marlowe Semantics by Alexander Nemish

- In `Language.Marlowe.Semantics`
- Data types
```haskell
data Contract = Close
              | Pay AccountId Payee Token (Value Observation) Contract
              | If Observation Contract Contract
              | When [Case Contract] Timeout Contract
              | Let ValueId (Value Observation) Contract
              | Assert Observation Contract

-- State of a Marlowe contract, stored on blockchain
data State = State { accounts    :: Accounts -- ^ Balances of accounts, by Party
                   , choices     :: Map ChoiceId ChosenNum -- ^ Choices which the parties made
                   , boundValues :: Map ValueId Integer -- ^ I.e. let bindings
                   , minSlot     :: Slot -- ^ First slot contract sees; allows to go back in time
                   }

-- Actions for a Marlowe contract
data Input = IDeposit AccountId Party Token Integer
           | IChoice ChoiceId ChosenNum
           | INotify

-- Input
data TransactionInput = TransactionInput
    { txInterval :: SlotInterval -- ^ Each input must a defined Slot interval
    , txInputs   :: [Input] } -- ^ Multiple inputs can be combined in one transaction, i.e. multiple deposits, choices or notifications

-- Output
data TransactionOutput =
    TransactionOutput
        { txOutWarnings :: [TransactionWarning]
        , txOutPayments :: [Payment] -- ^ Expected payments to happen
        , txOutState    :: State -- ^ Output state
        , txOutContract :: Contract -- ^ Output contract
        }
    | Error TransactionError

-- Final data which is stored on blockchain
data MarloweData = MarloweData
    { marloweState    :: State
    , marloweContract :: Contract
    }
```

- Function
```haskell
-- Entry point for semantics ("main" function)
computeTransaction :: TransactionInput -> State -> Contract -> TransactionOutput
computeTransaction tx state contract = let
    inputs = txInputs tx
    -- Check for errors in slot interval
    -- E.g. if timeouts are contained in interval
    in case fixInterval (txInterval tx) state of
        -- Apply all inputs
        IntervalTrimmed env fixState -> case applyAllInputs env fixState contract inputs of
            -- If successful: Return transaction output
            ApplyAllSuccess warnings payments newState cont ->
                    if (contract == cont) && ((contract /= Close) || (Map.null $ accounts state))
                    then Error TEUselessTransaction
                    else TransactionOutput { txOutWarnings = warnings
                                           , txOutPayments = payments
                                           , txOutState = newState
                                           , txOutContract = cont }
            ApplyAllNoMatchError -> Error TEApplyNoMatchError
            ApplyAllAmbiguousSlotIntervalError -> Error TEAmbiguousSlotIntervalError
        IntervalError error -> Error (TEIntervalError error)

-- Loop for applying inputs
applyAllInputs :: Environment -> State -> Contract -> [Input] -> ApplyAllResult
applyAllInputs env state contract inputs = let
    applyAllLoop
        :: Environment
        -> State
        -> Contract
        -> [Input]
        -> [TransactionWarning]
        -> [Payment]
        -> ApplyAllResult
    applyAllLoop env state contract inputs warnings payments =
        -- Reduces and...
        case reduceContractUntilQuiescent env state contract of
            RRAmbiguousSlotIntervalError -> ApplyAllAmbiguousSlotIntervalError
            -- ... gets state. Takes first input...
            ContractQuiescent reduceWarns pays curState cont -> case inputs of
                -- ... until all inputs are applied.
                -- Continue with continuation 'çont'.
                [] -> ApplyAllSuccess
                    (warnings ++ convertReduceWarnings reduceWarns)
                    (payments ++ pays)
                    curState
                    cont
                -- Processing the head input:
                -- - Applying input and
                -- - continuing loop 'applyAllLoop' with recursion
                (input : rest) -> case applyInput env curState input cont of
                    Applied applyWarn newState cont ->
                        applyAllLoop
                            env
                            newState
                            cont
                            rest
                            (warnings
                                ++ convertReduceWarnings reduceWarns
                                ++ convertApplyWarning applyWarn)
                            (payments ++ pays)
                    ApplyNoMatchError -> ApplyAllNoMatchError
    in applyAllLoop env state contract inputs [] []
  where
    convertApplyWarning :: ApplyWarning -> [TransactionWarning]
    convertApplyWarning warn = -- ...

-- Reduces loop.
reduceContractUntilQuiescent :: Environment -> State -> Contract -> ReduceResult
reduceContractUntilQuiescent env state contract = let
    reductionLoop
      :: Environment -> State -> Contract -> [ReduceWarning] -> [Payment] -> ReduceResult
    reductionLoop env state contract warnings payments =
        -- 'reduceContractStep' tries to reduce the contract, i.e. evaluates a step.
        -- All steps except 'When' in general reduces the contract.
        -- 'When' is only reduced if it is timed out.
        -- If not reduced, than the contract goes into state 'quiescent'.
        case reduceContractStep env state contract of
            Reduced warning effect newState cont -> let
                newWarnings = if warning == ReduceNoWarning then warnings
                              else warning : warnings
                newPayments  = case effect of
                    ReduceWithPayment payment -> payment : payments
                    ReduceNoPayment           -> payments
                in reductionLoop env newState cont newWarnings newPayments
            AmbiguousSlotIntervalReductionError -> RRAmbiguousSlotIntervalError
            -- this is the last invocation of reductionLoop, so we can reverse lists
            NotReduced -> ContractQuiescent (reverse warnings) (reverse payments) state contract

    in reductionLoop env state contract [] []

-- Evaluates a contract step.
reduceContractStep :: Environment -> State -> Contract -> ReduceStepResult
reduceContractStep env state contract = case contract of

```

- In essence: Marlowe contract evaluation has two steps
  1. Reduce current contract until it is quiescent
     - Meaning it is either
       - closed or
       - reached a `When` that is not timed out yet
  2. Try to apply inputs and move contract forward


### Client side

- Note: Semantics of Marlowe are abstract and do not depend on Cardano transaction mechanism
- Validator which executes code on-chain, `Language.Marlowe.Client`

```haskell
-- Entry point calls 'mkValidator' which creates a state machine.
-- Transition function is mkMarloweStateMachineTransition.
mkMarloweValidatorCode
    :: MarloweParams
    -> PlutusTx.CompiledCode (Scripts.ValidatorType MarloweStateMachine)
mkMarloweValidatorCode params =
    $$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode params

-- Create state machine.
-- Final state check: Check if current contract is 'Close'.
mkValidator :: MarloweParams -> Scripts.ValidatorType MarloweStateMachine
mkValidator p = SM.mkValidator $ SM.mkStateMachine Nothing (mkMarloweStateMachineTransition p) isFinal

-- Core for Marlowe contract validator.
mkMarloweStateMachineTransition
    :: MarloweParams
    -> SM.State MarloweData
    -> MarloweInput
    -> Maybe (TxConstraints Void Void, SM.State MarloweData)
mkMarloweStateMachineTransition params SM.State{ SM.stateData=MarloweData{..}, SM.stateValue=scriptInValue}
    (interval@(minSlot, maxSlot), inputs) = do
    -- Require balances to be in a positive state.
    let positiveBalances = validateBalances marloweState ||
            P.traceError "Invalid contract state. There exists an account with non positive balance"

    -- Produce constraints based on given 'inputs'.
    -- E.g. in case of deposits, expect that money flows into the contract.
    -- In case of choices, signatures are expected from parties.
    let inputsConstraints = validateInputs params inputs

    -- Check balance and if its expected balance.
    let inputBalance = totalBalance (accounts marloweState)
    let balancesOk = inputBalance == scriptInValue
    let preconditionsOk = P.traceIfFalse "Preconditions are false" $ positiveBalances && balancesOk

    -- Construct the transaction input
    let txInput = TransactionInput {
            txInterval = interval,
            txInputs = inputs }

    -- Call main function/entry point for semantics (seen above already): 'computeTransaction'.
    let computedResult = computeTransaction txInput marloweState marloweContract
    case computedResult of
        TransactionOutput {txOutPayments, txOutState, txOutContract} -> do

            -- Create new model data
            let marloweData = MarloweData {
                    marloweContract = txOutContract,
                    marloweState = txOutState }

            let (outputsConstraints, finalBalance) = case txOutContract of
                    Close -> (payoutContraints txOutPayments, P.zero)
                    _ -> let
                        -- Create constraints for pay outs to parties.
                        outputsConstraints = payoutContraints txOutPayments
                        totalIncome = P.foldMap collectDeposits inputs
                        totalPayouts = P.foldMap (\(Payment _ v) -> v) txOutPayments
                        -- Compute the new balance.
                        finalBalance = totalIncome P.- totalPayouts
                        in (outputsConstraints, finalBalance)
            let range = Interval.interval minSlot maxSlot
            -- Combine all constraints, along with range validation.
            let constraints = inputsConstraints <> outputsConstraints <> mustValidateIn range
            if preconditionsOk
            then Just (constraints, SM.State marloweData finalBalance)
            else Nothing
        Error _ -> Nothing

  where
    -- Validating inputs.
    validateInputs :: MarloweParams -> [Input] -> TxConstraints Void Void
    validateInputs MarloweParams{rolesCurrency} inputs = let
        (keys, roles) = P.foldMap validateInputWitness inputs
        mustSpendSetOfRoleTokens = P.foldMap mustSpendRoleToken (AssocMap.keys roles)
        -- Check that signatures exist of parties (by public key hashes)
        -- along with check for spending role tokens.
        in P.foldMap mustBeSignedBy keys P.<> mustSpendSetOfRoleTokens
      where
        validateInputWitness :: Input -> ([PubKeyHash], AssocMap.Map TokenName ())
        validateInputWitness input =
            case input of
                IDeposit _ party _ _         -> validatePartyWitness party
                IChoice (ChoiceId _ party) _ -> validatePartyWitness party
                INotify                      -> (P.mempty, P.mempty)
          where
            validatePartyWitness (PK pk)     = ([pk], P.mempty)
            validatePartyWitness (Role role) = ([], AssocMap.singleton role ())

        mustSpendRoleToken :: TokenName -> TxConstraints Void Void
        mustSpendRoleToken role = mustSpendAtLeast $ Val.singleton rolesCurrency role 1

    collectDeposits :: Input -> Val.Value
    collectDeposits (IDeposit _ _ (Token cur tok) amount) = Val.singleton cur tok amount
    collectDeposits _                                     = P.zero

    payoutContraints :: [Payment] -> TxConstraints i0 o0
    payoutContraints payments = P.foldMap paymentToTxOut paymentsByParty
      where
        paymentsByParty = AssocMap.toList $ P.foldMap paymentByParty payments

        -- Payments to party must either
        paymentToTxOut (party, value) = case party of
            -- go to a public key or
            PK pk  -> mustPayToPubKey pk value
            Role role -> let
                dataValue = Datum $ PlutusTx.toData role
                -- to a role payout hash.
                -- 'rolePayoutValidatorHash' can be customized.
                -- Default one checks that role token is spent.
                in mustPayToOtherScript (rolePayoutValidatorHash params) dataValue value

        paymentByParty (Payment party money) = AssocMap.singleton party money


```

### Off-chain execution

- Three Marlowe contracts offered
  - `marloweFollowContract`
  - `marloweControlContract`
  - `marloweCompanionContract`

```haskell
marloweFollowContract :: Contract ContractHistory MarloweFollowSchema MarloweError ()
marloweFollowContract = do
    -- Only endpoint
    params <- endpoint @"follow"
    slot <- currentSlot
    logDebug @String "Getting contract history"
    -- Basically subscribes to a validator, i.e. Marlowe contract, address
    -- in order to get all changes for the address (all transactions).
    follow 0 slot params
  where
    follow ifrom ito params = do
        let client@StateMachineClient{scInstance} = mkMarloweClient params
        let inst = validatorInstance scInstance
        let address = Scripts.scriptAddress inst
        -- Start subscription, receive changes
        AddressChangeResponse{acrTxns} <- addressChangeRequest
                AddressChangeRequest
                { acreqSlotRangeFrom = ifrom
                , acreqSlotRangeTo = ito
                , acreqAddress = address
                }
        let go [] = pure InProgress
            go (tx:rest) = do
                -- Store all inputs which are applied to contract.
                -- Current state can be reproduced by applying all inputs (collected in 'ContractHistory' to initial state).
                -- Note:
                -- - Uses tell to add inputs to state.
                -- - E.g. if connected via WebSocket, get live updates of changes.
                -- - Used for example by Marlowe Run
                res <- updateHistoryFromTx client params tx
                case res of
                    Finished   -> pure Finished
                    InProgress -> go rest
        res <- go acrTxns
        case res of
            Finished -> do
                logDebug @String ("Contract finished " <> show params)
                pure () -- close the contract
            InProgress ->
                let next = succ ito in
                follow next next params

    updateHistoryFromTx StateMachineClient{scInstance, scChooser} params tx = do
      -- ...


-- Control contract
marlowePlutusContract :: Contract MarloweContractState MarloweSchema MarloweError ()
marlowePlutusContract = do
  -- Allows creation, applying inputs, auto execute the contract if possible, redeem tokens from payments to roles/your role or close.
    create `select` apply `select` auto `select` redeem `select` close
  where
    -- Create endpoints
    create = do
        -- Provide contract and map from roles to public keys.
        (owners, contract) <- endpoint @"create"
        -- Parametrize Marlowe contract. Params allows to set own 'rolePayoutValidatorHash' script and requires a currency symbol.
        -- 'setupMarloweParams':
        -- - Searches for owners with roles
        -- - Create tokens for roles by using 'Currency.forgeContract'
        -- - All roles initially owned by creator
        -- - In same transaction as contract creation, distribute role tokens to supppoed owners
        (params, distributeRoleTokens) <- setupMarloweParams owners contract
        slot <- currentSlot
        -- Create the state machine client
        let StateMachineClient{scInstance} = mkMarloweClient params
        let marloweData = MarloweData {
                marloweContract = contract,
                marloweState = emptyState slot }
        let payValue = adaValueOf 0
        let StateMachineInstance{validatorInstance} = scInstance
        -- Create the transaction which also sets the state data
        let tx = mustPayToTheScript marloweData payValue <> distributeRoleTokens
        let lookups = Constraints.scriptInstanceLookups validatorInstance
        utx <- either (throwing _ConstraintResolutionError) pure (Constraints.mkTx lookups tx)
        -- Submit Marlowe contract to chain
        submitTxConfirmed utx
        marlowePlutusContract
    -- Apply endpoint
    apply = do
        (params, slotInterval, inputs) <- endpoint @"apply-inputs"
        -- Just creates a valid slot range and runs 'runStep' with state machine client
        _ <- applyInputs params slotInterval inputs
        marlowePlutusContract
    --
    redeem = mapError (review _MarloweError) $ do
        (MarloweParams{rolesCurrency}, role, pkh) <-
            endpoint @"redeem"
        -- Get address of role currency
        let address = scriptHashAddress (mkRolePayoutValidatorHash rolesCurrency)
        utxos <- utxoAt address
        let spendPayoutConstraints tx ref TxOutTx{txOutTxOut} = let
                expectedDatumHash = datumHash (Datum $ PlutusTx.toData role)
                amount = txOutValue txOutTxOut
                in case txOutDatum txOutTxOut of
                    Just datumHash | datumHash == expectedDatumHash ->
                        Constraints.mustSpendScriptOutput ref unitRedeemer
                        -- Spend all outputs to token owner
                            <> Constraints.mustPayToPubKey pkh amount
                    _ -> tx

        let spendPayouts = Map.foldlWithKey spendPayoutConstraints mempty utxos
            constraints = spendPayouts
                -- must spend a role token for authorization
                <> Constraints.mustSpendAtLeast (Val.singleton rolesCurrency role 1)
            -- lookup for payout validator and role payouts
            validator = rolePayoutScript rolesCurrency
            lookups = Constraints.otherScript validator
                <> Constraints.unspentOutputs utxos
                <> Constraints.ownPubKeyHash pkh
        tx <- either (throwing _ConstraintResolutionError) pure (Constraints.mkTx @Void lookups constraints)
        _ <- submitUnbalancedTx tx
        marlowePlutusContract
    -- Some contracts can be executed automatically
    -- E.g. contracts without requiring parties to make choices. So only scheduled payments.
    auto = do
        (params, party, untilSlot) <- endpoint @"auto"
        let theClient = mkMarloweClient params
        let continueWith :: MarloweData -> Contract MarloweContractState MarloweSchema MarloweError ()
            continueWith md@MarloweData{marloweContract} =
                if canAutoExecuteContractForParty party marloweContract
                -- Execute auto execute.
                -- In essence a state machine which pays own deposits or wait for other parties to fulfill their part of the contract
                then autoExecuteContract theClient party md
                else marlowePlutusContract

        maybeState <- SM.getOnChainState theClient
        case maybeState of
            Nothing -> do
                wr <- SM.waitForUpdateUntil theClient untilSlot
                case wr of
                    ContractEnded -> do
                        logInfo @String $ "Contract Ended for party " <> show party
                        marlowePlutusContract
                    Timeout _ -> do
                        logInfo @String $ "Contract Timeout for party " <> show party
                        marlowePlutusContract
                    WaitingResult marloweData -> continueWith marloweData
            Just ((st, _), _) -> do
                let marloweData = tyTxOutData st
                continueWith marloweData
    close = endpoint @"close"

-- Monitors participants wallet and notifies if a role token goes to own address.
marloweCompanionContract :: Contract CompanionState MarloweCompanionSchema MarloweError ()
marloweCompanionContract = contracts
  where
    contracts = do
        pkh <- pubKeyHash <$> ownPubKey
        let ownAddress = pubKeyHashAddress pkh
        utxo <- utxoAt ownAddress
        let txOuts = fmap (txOutTxOut . snd) $ Map.toList utxo
        forM_ txOuts notifyOnNewContractRoles
        cont ownAddress
    cont ownAddress = do
        txns <- nextTransactionsAt ownAddress
        let txOuts = txns >>= eitherTx (const []) txOutputs
        -- 'notifyOnNewContractRoles':
        -- - Filter for roles
        -- - Find the Marlowe contract for the role
        -- - If succeeds, update state of contract
        -- If subscribed e.g. via WebSocket, a notification is received.
        forM_ txOuts notifyOnNewContractRoles
        cont ownAddress
```



## Demo

- [Marlowe Alpha Playground](https://alpha.marlowe.iohkdev.io/#/)
- E.g. with blockly.
- Example contract
  - Alice and Bob deposit ADA in contract
  - Charlie decides which one of the two receives total amount
  - If anything goes wrong: Alice and Bob get back what they deposited
- Note:
  - There are contract interal accounts
  - Use roles which are inhabited by public keys for representing Alice, Bob and Charlie
  - On Close, internal balances are payed out
  - Contract defined via Blockly can be used directly within Haskell
    - But Haskell allows to use full power of Haskell
    - E.g. avoid defining `Role "Alice" :: Party` repeatedly
    - Or allow also Bob to deposit first as well: create helper function for the `When` block, which flips the one who deposits first



## Homework

Modify contract
- Charlie needs to provide double deposit first
- If he does not choose winner, Alice and Bob get half of deposit from Charlie
- Otherwise he gets his deposit
