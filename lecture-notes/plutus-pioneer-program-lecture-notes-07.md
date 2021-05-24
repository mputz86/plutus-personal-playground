---
date: 2021-05-24T12:43
---

# Cardano Plutus Pioneer Program: Lecture 7

[Video](https://www.youtube.com/watch?v=oJupInqvJUI)

- Topic: State Machines
  - Supported in Plutus library
  - Builds on top of lower level basics learned so far

- Example: Game with (B)ob and (A)lice
```
      Bob
    | 0 1
  --+----
A 0 | A B
  1 | B A
```
  - Like Rock-Paper-Scissors
  - Gesture for 0 and 1
  - If equal: A wins, else B

- Situation: Not in same room
  - Play via eMail does not work
  - A sends her gesture to B
  - B knows and can always answer so that he wins

- Cryptographic trick: Commit schemes
  - A does not reveal choice, but commits to it
  - Done via hash functions

- Hash functions = one way functions
  - Given hash, hard/impossible to get original bytestring
  - So A sends hash to B (like `hash(0) = ff27...`)
  - B does not know what A has chosen
  - Requires one additional step
    - A needs to send in 3rd step plain her choice
    - B can verify that A sent her original choice:
      - Computes hash on his own: `hash(0) ?= ff27...`
      - Compares with A hash from step 1

- Problem here:
  - Here only two choices, so hashes are `hash(0)` and `hash(1)` which will be always the same
  - B can tell what A has chosen by hash
  - Solution: Combine a `nonce` (arbitrary generated data by A) with choice `hash(nonce || 0)`
    - B cannot predict choice of A anymore
    - A must reveal `nonce` as well in 3rd step
    - B can verify


## State Machine

- Flow / State Machine
  - Alice sends `Hash`
  - Bob plays by sending his choice `c_b`
  - Two options:
    1. Alice has won, so she reveals her choice and wins
    2. If Alices has not won, after a timeout, Bob able to Claim to be the winner
      - Alice has no incentive to `Reveal`
  - If Bob does not play after Alice has played: Alice must be able to claim her money back
```

Alice           Bob
------> [Hash] -------> [Hash, c_b] ---+
         |         Play     |        Bob | Claim
   Alice | Claim            |            |
         |            Alice | Reveal     |
         |                  |            |
         +------------------+------------+-->O (final state)
```


## Implementation (without using State Machines)

- Module `Week07.EvenOdd`

- Validator parameter `Game` and `GameChoice`s
  - UTxO with token used to track state of game
```haskell
data Game = Game
    { gFirst          :: !PubKeyHash -- ^ Alice
    , gSecond         :: !PubKeyHash -- ^ Bob
    , gStake          :: !Integer    -- ^ Lovelace each has to put into game
    , gPlayDeadline   :: !Slot       -- ^ Time until second player can make move before first can claim back money
    , gRevealDeadline :: !Slot       -- ^ Time until first player can claim victory
    , gToken          :: !AssetClass -- ^ NFT used to identify right UTxO
    } -- ...


data GameChoice = Zero | One
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

-- Required to write for Plutus
instance Eq GameChoice where
    -- Required to be added to be useable in on-chain code
    {-# INLINABLE (==) #-}
    Zero == Zero = True
    One  == One  = True
    _    == _    = False
```

- State of Game
  - `ByteString` is hash of first player
  - `Maybe GameChoice` is choice of second player
```haskell
data GameDatum = GameDatum ByteString (Maybe GameChoice)
```

- Redeemer
  - States
```haskell
data GameRedeemer
  = Play GameChoice   -- ^ 2nd players choice
  | Reveal ByteString -- ^ 1st player reveals his choice by providing nonce (his choice is clear because of 2nd players choice)
  | ClaimFirst        -- ^ 1st player can back money because 2nd does not play
  | ClaimSecond       -- ^ 2nd player can get money, because 1st has lost and does not reveal
```

- Validator
  - `Game` are parameters of game
  - Two `ByteString` are represenations for choice 0 and 1
    - Because can not use bytestring literals in Plutus
  - `checkNonce`
    - First `ByteString` is submitted hash
    - Revealed nonce
    - Choice of 2nd player (must be same as choice of 1st, otherwise first player does not win)
    - Check hash
```haskell
sha2_256 (nonce `concatenate` cFirst) == bs
```
  - Token/NFT goes back to first player since he initialized game with it
  - Conditions
    - For all: Input which is validated must contain state token
    - Pattern match on different states
```haskell
        -- 2nd player is moving (has not moved yet)
        (GameDatum bs Nothing, Play c) ->
            traceIfFalse "not signed by second player"   (txSignedBy info (gSecond game))                                   &&
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&
            traceIfFalse "second player's stake missing" (lovelaces (txOutValue ownOutput) == (2 * gStake game))            &&
            traceIfFalse "wrong output datum"            (outputDatum == GameDatum bs (Just c))                             &&
            traceIfFalse "missed deadline"               (to (gPlayDeadline game) `contains` txInfoValidRange info)         &&
            traceIfFalse "token missing from output"     (assetClassValueOf (txOutValue ownOutput) (gToken game) == 1)

        -- 1st player has won, reveals his nonce in order to claim
        (GameDatum bs (Just c), Reveal nonce) ->
            traceIfFalse "not signed by first player"    (txSignedBy info (gFirst game))                                    &&
            traceIfFalse "commit mismatch"               (checkNonce bs nonce c)                                            &&
            traceIfFalse "missed deadline"               (to (gRevealDeadline game) `contains` txInfoValidRange info)       &&
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&
            traceIfFalse "NFT must go to first player"   nftToFirst

        -- 2nd player has not moved before `gPlayDeadline`, so 1st can claim back money
        (GameDatum _ Nothing, ClaimFirst) ->
            traceIfFalse "not signed by first player"    (txSignedBy info (gFirst game))                                    &&
            traceIfFalse "too early"                     (from (1 + gPlayDeadline game) `contains` txInfoValidRange info)   &&
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&
            traceIfFalse "NFT must go to first player"   nftToFirst

        -- Both have moved, 1st has lost, so 2nd can claim stake
        (GameDatum _ (Just _), ClaimSecond) ->
            traceIfFalse "not signed by second player"   (txSignedBy info (gSecond game))                                   &&
            traceIfFalse "too early"                     (from (1 + gRevealDeadline game) `contains` txInfoValidRange info) &&
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&
            traceIfFalse "NFT must go to first player"   nftToFirst

        _                              -> False

```
- Off-chain code
```haskell
-- Finds correct UTxO with token and game state
findGameOutput :: HasBlockchainActions s => Game -> Contract w s Text (Maybe (TxOutRef, TxOutTx, GameDatum))
findGameOutput game = do
    utxos <- utxoAt $ gameAddress game
    return $ do
        (oref, o) <- find f $ Map.toList utxos
        dat       <- gameDatum (txOutTxOut o) (`Map.lookup` txData (txOutTxTx o))
        return (oref, o, dat)
  where
    f :: (TxOutRef, TxOutTx) -> Bool
    f (_, o) = assetClassValueOf (txOutValue $ txOutTxOut o) (gToken game) == 1

-- Params for game initialization, 1st player
data FirstParams = FirstParams
    { fpSecond         :: !PubKeyHash     -- ^ Other player
    , fpStake          :: !Integer
    , fpPlayDeadline   :: !Slot
    , fpRevealDeadline :: !Slot
    , fpNonce          :: !ByteString     -- ^ Used Nonce
    , fpCurrency       :: !CurrencySymbol
    , fpTokenName      :: !TokenName
    , fpChoice         :: !GameChoice     -- ^ Choice player makes
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

```

- Contract for first player
```haskell
firstGame :: forall w s. HasBlockchainActions s => FirstParams -> Contract w s Text ()
firstGame fp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let game = Game
            { gFirst          = pkh
            , gSecond         = fpSecond fp
            , gStake          = fpStake fp
            , gPlayDeadline   = fpPlayDeadline fp
            , gRevealDeadline = fpRevealDeadline fp
            , gToken          = AssetClass (fpCurrency fp, fpTokenName fp)
            }
        -- Add stake
        v    = lovelaceValueOf (fpStake fp) <> assetClassValue (gToken game) 1
        c    = fpChoice fp
        -- Commitment to choice
        bs   = sha2_256 $ fpNonce fp `concatenate` if c == Zero then bsZero else bsOne
        tx   = Constraints.mustPayToTheScript (GameDatum bs Nothing) v
    ledgerTx <- submitTxConstraints (gameInst game) tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "made first move: " ++ show (fpChoice fp)

    void $ awaitSlot $ 1 + fpPlayDeadline fp

    m <- findGameOutput game
    case m of
        Nothing             -> throwError "game output not found"
        Just (oref, o, dat) -> case dat of
            -- Reclaim if 2nd player did not play
            GameDatum _ Nothing -> do
                logInfo @String "second player did not play"
                let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                              Constraints.otherScript (gameValidator game)
                    tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData ClaimFirst)
                ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                void $ awaitTxConfirmed $ txId ledgerTx'
                logInfo @String "reclaimed stake"

            -- Reveal if 2nd player lost in order to win stake
            GameDatum _ (Just c') | c' == c -> do
                logInfo @String "second player played and lost"
                let lookups = Constraints.unspentOutputs (Map.singleton oref o)                                         <>
                              Constraints.otherScript (gameValidator game)
                    tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData $ Reveal $ fpNonce fp) <>
                              Constraints.mustValidateIn (to $ fpRevealDeadline fp)
                ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                void $ awaitTxConfirmed $ txId ledgerTx'
                logInfo @String "victory"

            _ -> logInfo @String "second player played and won"
```

- Contract for 2nd player
```haskell
-- Similar to 'FirstParams', but no nonce required
data SecondParams = SecondParams
    { spFirst          :: !PubKeyHash
    , spStake          :: !Integer
    , spPlayDeadline   :: !Slot
    , spRevealDeadline :: !Slot
    , spCurrency       :: !CurrencySymbol
    , spTokenName      :: !TokenName
    , spChoice         :: !GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

secondGame :: forall w s. HasBlockchainActions s => SecondParams -> Contract w s Text ()
secondGame sp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let game = Game
            { gFirst          = spFirst sp
            , gSecond         = pkh
            , gStake          = spStake sp
            , gPlayDeadline   = spPlayDeadline sp
            , gRevealDeadline = spRevealDeadline sp
            , gToken          = AssetClass (spCurrency sp, spTokenName sp)
            }
    m <- findGameOutput game
    case m of
        -- Game found, add own choice
        Just (oref, o, GameDatum bs Nothing) -> do
            logInfo @String "running game found"
            let token   = assetClassValue (gToken game) 1
                -- Add own stake
            let v       = let x = lovelaceValueOf (spStake sp) in x <> x <> token
                c       = spChoice sp
                lookups = Constraints.unspentOutputs (Map.singleton oref o)                            <>
                          Constraints.otherScript (gameValidator game)                                 <>
                          Constraints.scriptInstanceLookups (gameInst game)
                tx      = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData $ Play c) <>
                          Constraints.mustPayToTheScript (GameDatum bs $ Just c) v                     <>
                          Constraints.mustValidateIn (to $ spPlayDeadline sp)
            ledgerTx <- submitTxConstraintsWith @Gaming lookups tx
            let tid = txId ledgerTx
            void $ awaitTxConfirmed tid
            logInfo @String $ "made second move: " ++ show (spChoice sp)

            -- Wait until reveal deadline has passed before doing something
            void $ awaitSlot $ 1 + spRevealDeadline sp

            m' <- findGameOutput game
            case m' of
                -- First player has won and has claimed already stake
                Nothing             -> logInfo @String "first player won"
                -- Fist player has either lost or left the game: 2nd player can claim stake
                Just (oref', o', _) -> do
                    logInfo @String "first player didn't reveal"
                    let lookups' = Constraints.unspentOutputs (Map.singleton oref' o')                              <>
                                   Constraints.otherScript (gameValidator game)
                        tx'      = Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toData ClaimSecond) <>
                                   Constraints.mustValidateIn (from $ 1 + spRevealDeadline sp)                      <>
                                   Constraints.mustPayToPubKey (spFirst sp) token
                    ledgerTx' <- submitTxConstraintsWith @Gaming lookups' tx'
                    void $ awaitTxConfirmed $ txId ledgerTx'
                    logInfo @String "second player won"

        _ -> logInfo @String "no running game found"
```

## State Machine

### Definition

- Definition
  - State shown as `O` (circle)
  - Directed transitions between states, shown as `-->` (arrows)
  - Special state:
    - Initial state has no ingoing arrows, shown as `🞊 ` (circle with smaller, filled, circle inside)
    - Final state has no outgoing arrows, shown as `⌾ ` (circle with smaller circle inside)
  - Is a directed graph
- See diagram with game transitions
- In blockchain
  - State machine represented as UTxO, sitting at a script address
  - State is `Datum` of UTxO
  - Transition is a `Transaction`
    - Consuming current state of UTxO by
    - using a `Redeemer`, which characterizes the transitions and
    - produces a new UTxO at same script address
    - where `Datum` represents new state
- Support in Plutus library for this
  - Module `Plutus.Contract.StateMachine`
    - `s` = state, `Datum`
    - `i` = input, `Redeemer`
```haskell
data StateMachine s i = StateMachine {
      -- | The transition function of the state machine. 'Nothing' indicates an invalid transition from the current state.
      smTransition  :: State s -> i -> Maybe (TxConstraints Void Void, State s),

      -- | Check whether a state is the final state
      smFinal       :: s -> Bool,

      -- | The condition checking function. Can be used to perform
      --   checks on the pending transaction that aren't covered by the
      --   constraints. 'smCheck' is always run in addition to checking the
      --   constraints, so the default implementation always returns true.
      smCheck       :: s -> i -> ScriptContext -> Bool,

      -- | The 'AssetClass' of the thread token that identifies the contract
      --   instance.
      smThreadToken :: Maybe AssetClass
    }

```

### Implementation of the game

- Add a `Final` state to `GameDatum`
- Now: `transition` functions derived from former validator function
  - No more check for token required, already checked within `StateMachine`
  - Uses the `mustBeSignedBy`, ... contrains already used in off-chain code
  - Check for nonce can not be expressed as a constraint (second case):
    - Added to `smCheck` function, function `check`

```haskell
transition :: Game -> State GameDatum -> GameRedeemer -> Maybe (TxConstraints Void Void, State GameDatum)
transition game s r = case (stateValue s, stateData s, r) of
    (v, GameDatum bs Nothing, Play c)
        | lovelaces v == gStake game         -> Just ( Constraints.mustBeSignedBy (gSecond game)                    <>
                                                       Constraints.mustValidateIn (to $ gPlayDeadline game)
                                                     , State (GameDatum bs $ Just c) (lovelaceValueOf $ 2 * gStake game)
                                                     )
    (v, GameDatum _ (Just _), Reveal _)
        | lovelaces v == (2 * gStake game)   -> Just ( Constraints.mustBeSignedBy (gFirst game)                     <>
                                                       Constraints.mustValidateIn (to $ gRevealDeadline game)       <>
                                                       Constraints.mustPayToPubKey (gFirst game) token
                                                     , State Finished mempty
                                                     )
    (v, GameDatum _ Nothing, ClaimFirst)
        | lovelaces v == gStake game         -> Just ( Constraints.mustBeSignedBy (gFirst game)                     <>
                                                       Constraints.mustValidateIn (from $ 1 + gPlayDeadline game)   <>
                                                       Constraints.mustPayToPubKey (gFirst game) token
                                                     , State Finished mempty
                                                     )
    (v, GameDatum _ (Just _), ClaimSecond)
        | lovelaces v == (2 * gStake game)   -> Just ( Constraints.mustBeSignedBy (gSecond game)                    <>
                                                       Constraints.mustValidateIn (from $ 1 + gRevealDeadline game) <>
                                                       Constraints.mustPayToPubKey (gFirst game) token
                                                     , State Finished mempty
                                                     )
    _                                        -> Nothing

```
- Defining the `StateMachine`
  - Requires a function for final states, `final`
```haskell
gameStateMachine :: Game -> ByteString -> ByteString -> StateMachine GameDatum GameRedeemer
gameStateMachine game bsZero' bsOne' = StateMachine
    { smTransition  = transition game
    , smFinal       = final
    , smCheck       = check bsZero' bsOne'
    , smThreadToken = Just $ gToken game
    }

```
  - Create validator from it with `mkValidator $ gameStateMachine ...`
```haskell
mkGameValidator :: Game -> ByteString -> ByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game bsZero' bsOne' = mkValidator $ gameStateMachine game bsZero' bsOne'
```

- Create a `StateMachineClient`
  - Interact from Wallet with state machine (`Contract` monad)
  - Takes a `StateMachineInstance` and a
  - `StateMachine` chooser
    - Defaults to token based approach
    - Able to do it differently and choose from all available UTxOs the right one
  - => Use `mkStateMachineClient :: StateMachineInstance state input -> StateMachineClinet state input`


- New `firstGame` version
  - Create a `client` with `gameClient`
  - Start state machine with `runInitialise` (with client, datum and value)
    - Value only contains stake of 1st player
  - Use `getOnChainState :: StateMachineClient state i -> Contract .. (Maybe (OnChainState state i, UtxoMap))`
    - If state found, returns `OnChainState`: `type OnChainState :: (TypedScriptTxOut (StateMachine s i), TypedScriptTxOutRef (StateMachine s i))`
      - Similar to what `utxoMap` returns
      - But typed
      - Also provides `Datum`, i.e. no manual searching and checking
  - Allows to directly use datum with `tyTxOutData`
  - In state pattern match:
    - Use `runStep` to move state machine forward, just `Redeemer` required
      - Takes `StateMachineClient`, `input` (Redeemer)
      - Submits transaction etc
        - No lookups, less helper functions, ...
        - Reduces code heavily
        - No duplicated logic anymore
      - Internal: Uses `Constraints` from transition function

```haskell
firstGame :: forall w s. HasBlockchainActions s => FirstParams -> Contract w s Text ()
firstGame fp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let game   = Game ...
        client = gameClient game
        v      = lovelaceValueOf (fpStake fp)
        c      = fpChoice fp
        bs     = sha2_256 $ fpNonce fp `concatenate` if c == Zero then bsZero else bsOne
    void $ mapError' $ runInitialise client (GameDatum bs Nothing) v
    logInfo @String $ "made first move: " ++ show (fpChoice fp)

    void $ awaitSlot $ 1 + fpPlayDeadline fp

    m <- mapError' $ getOnChainState client
    case m of
        Nothing             -> throwError "game output not found"
        Just ((o, _), _) -> case tyTxOutData o of

            GameDatum _ Nothing -> do
                logInfo @String "second player did not play"
                void $ mapError' $ runStep client ClaimFirst
                logInfo @String "first player reclaimed stake"

            GameDatum _ (Just c') | c' == c -> do
                logInfo @String "second player played and lost"
                void $ mapError' $ runStep client $ Reveal $ fpNonce fp
                logInfo @String "first player revealed and won"

            _ -> logInfo @String "second player played and won"

-- ...

secondGame :: forall w s. HasBlockchainActions s => SecondParams -> Contract w s Text ()
secondGame sp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let game   = Game ...
        client = gameClient game
    m <- mapError' $ getOnChainState client
    case m of
        Nothing          -> logInfo @String "no running game found"
        Just ((o, _), _) -> case tyTxOutData o of
            GameDatum _ Nothing -> do
                logInfo @String "running game found"
                void $ mapError' $ runStep client $ Play $ spChoice sp
                logInfo @String $ "made second move: " ++ show (spChoice sp)

                void $ awaitSlot $ 1 + spRevealDeadline sp

                m' <- mapError' $ getOnChainState client
                case m' of
                    Nothing -> logInfo @String "first player won"
                    Just _  -> do
                        logInfo @String "first player didn't reveal"
                        void $ mapError' $ runStep client ClaimSecond
                        logInfo @String "second player won"

            _ -> throwError "unexpected datum"
```

- Note:
  - On-chain checks if transaction is valid
  - Off-chain constructs transaction
  - So different purposes
  - But by defining via constraints, used for both

- State machines not always appropriate
  - But if: Use them
  - Avoid duplication, potential errors/bugs, ...


## Homework

- Modify `StateMachie` module and implement `RockPaperScissors` game as state machine
```haskell
-- Now three choices
data GameChoice = Rock | Paper | Scissors

-- Recommended to implement
beats :: GameChoice -> GameChoice -> Bool
beats Rock Scissors = True
beats Paper Rock = True
beats Scissors Paper = True
beats _ _ = False

-- Redeemer also needs GameChoice of player one
data GameRedeemer = Play GameChoice | Reveal ByteString GameChoice | ClaimFirst | ClaimSecond
```

  - Note: Draw is possible, then each player should get his stake back
  - One more case in transition function
  - Some more changes, but a lot similar
