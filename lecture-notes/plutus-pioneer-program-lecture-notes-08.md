---
date: 2021-05-29T14:11
---

# Cardano Plutus Pioneer Program: Lecture 8

- Recap:
  - Last lecture StateMachines
  - Less code to express logic of smart contract
  - Since lot of sharing in on- and off-chain code (i.e. TxConstraints)
  - Plus boilerplate captured in StateMachine machinery
    - Like NFT token handling

- This lecture
  - Another State Machine example
  - And testing

## Example: Token Sell

- Seller wants to sell Tokens
- Locks NFT at Script address (for identifying correct UTxO)
  - Script called TS (Token Sale)
  - Datum: Price of token, initialized with 0
- Seller can update price of token with `setPrice`
  - Consumes UTxO with token
- Seller can `addTokens`:
  - Provide UTxO with Tokens
  - Consume TS UTxO
- Buyer
  - Adds Tx which
    - consumes the tokens
    - And provides UTxO with price for tokens
  - Outputs:
    - Updated contract state
      - Less tokens, Amount of ADA
    - UTxO to buyer with just bought tokens

- Seller
  - Can retriebe ADA and/or tokens
  - Updates contract state
  - UTxO to seller with retrieved tokens and ADA

- Just one flow
  - Actions can happen in different order

```
            ┌──────┐                     ┌──────┐
            │      │                     │      │
Seller      │      │    TS    set price  │      │    TS
───────O────┤ TX 1 ├───────O─────────────┤ TX 2 ├───────┐
  NFT       │      │   NFT               │      │   NFT │
            │      │    0                │      │    6  │
            └──────┘                     └──────┘       │
                            add tokens                  │
         ┌──────────────────────────────────────────O───┘
         │
         │  ┌──────┐
         │  │      │
         │  │      │  TS
         └──┤ TX 3 ├───────────O
  Seller    │      │ NFT + 5T  │
────────────┤      │  6        │
    5T      └──────┘           │
                               │
                               │
         ┌─────────────────────┘            ┌──────┐    TS
         │                                  │      ├───────O
         │  ┌──────┐  TS           withdraw │      │   NFT + 2T
         │  │      ├────────────O───────────┤ TX 5 │    6
         │  │      │ NFT + 3T + 12 Ada      │      │
         └──┤ TX 4 │  6                     │      ├────┐
  Buyer     │      │                        └──────┘    │
────────────┤      ├──────┐                             │ Seller
   12 Ada   └──────┘      │                             └────────────O
                          │  Buyer                         1T + 12Ada
                          └──────────O
                              2T
```
- Contract params
```haskell
data TokenSale = TokenSale
    { tsSeller :: !PubKeyHash
    , tsToken  :: !AssetClass
    , tsNFT    :: !AssetClass
    }
```

- Redeemer
```haskell
data TSRedeemer =
      SetPrice Integer
    | AddTokens Integer
    | BuyTokens Integer
    | Withdraw Integer Integer -- ^ First: How many tokens, Second: How many Lovelaces
    deriving (Show, Prelude.Eq)

```

- Transition function
  - `State Integer` is price for token
  - For all cases:
    - `v` on left hand side (in pattern) does contain NFT
    - But on right hand side dont want to add it, so needed to add `nft (negate 1)`
    - Avoid negative values for amounts of tokens or Lovelaces
    - NFT supposed to stay forever in contract
  - `AddTokens`:
    - No need to check if only Seller since
    - Seller does not care if somebody else provides tokens to his contract
    - Just sells them and sees them as gift
    - So no constraints: `mempty`
```haskell
transition :: TokenSale -> State Integer -> TSRedeemer -> Maybe (TxConstraints Void Void, State Integer)
transition ts s r = case (stateValue s, stateData s, r) of
    (v, _, SetPrice p)   | p >= 0           -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                    , State p $
                                                      v <>
                                                      nft (negate 1)
                                                    )
    (v, p, AddTokens n)  | n > 0            -> Just ( mempty
                                                    , State p $
                                                      v              <>
                                                      nft (negate 1) <>
                                                      assetClassValue (tsToken ts) n
                                                    )
    (v, p, BuyTokens n)  | n > 0            -> Just ( mempty
                                                    , State p $
                                                      v                                       <>
                                                      nft (negate 1)                          <>
                                                      assetClassValue (tsToken ts) (negate n) <>
                                                      lovelaceValueOf (n * p)
                                                    )
    (v, p, Withdraw n l) | n >= 0 && l >= 0 -> Just ( Constraints.mustBeSignedBy (tsSeller ts)
                                                    , State p $
                                                      v                                       <>
                                                      nft (negate 1)                          <>
                                                      assetClassValue (tsToken ts) (negate n) <>
                                                      lovelaceValueOf (negate l)
                                                    )
    _                                       -> Nothing
  where
    nft :: Integer -> Value
    nft = assetClassValue (tsNFT ts)

```
  - Since no additional checks beside constraints, use `mkStateMachine`
    - Provide NFT, `transition` function and
    - Final states: `const False` equals to no final state, runs forever
```haskell
tsStateMachine :: TokenSale -> StateMachine Integer TSRedeemer
tsStateMachine ts = mkStateMachine (Just $ tsNFT ts) (transition ts) (const False)
```
  - Create client with `mkStateMachineClient`
```haskell
tsClient :: TokenSale -> StateMachineClient Integer TSRedeemer
tsClient ts = mkStateMachineClient $ StateMachineInstance (tsStateMachine ts) (tsInst ts)
```


- Contract `startTS`
  - Allows to provide an NFT or create one ad-hoc by expecting a `Maybe CurrencySymbol`
  - Use `Contract (Last TokenSale)` so other contracts can find it
```haskell
startTS :: HasBlockchainActions s => Maybe CurrencySymbol -> AssetClass -> Contract (Last TokenSale) s Text TokenSale
startTS mcs token = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    cs  <- case mcs of
        Nothing  -> C.currencySymbol <$> mapErrorC (C.forgeContract pkh [(nftName, 1)])
        Just cs' -> return cs'
    let ts = TokenSale
            { tsSeller = pkh
            , tsToken  = token
            , tsNFT    = AssetClass (cs, nftName)
            }
        client = tsClient ts
    void $ mapErrorSM $ runInitialise client 0 mempty
    tell $ Last $ Just ts
    logInfo $ "started token sale " ++ show ts
    return ts
```

- Since perfect fit, all other contracts short
  - Just create `Redeemer` and call `runStep` for state machine
  - All functions requires the `TokenSale`, which is returend by `startTS` with `tell`
```haskell
setPrice :: HasBlockchainActions s => TokenSale -> Integer -> Contract w s Text ()
setPrice ts p = void $ mapErrorSM $ runStep (tsClient ts) $ SetPrice p

addTokens :: HasBlockchainActions s => TokenSale -> Integer -> Contract w s Text ()
addTokens ts n = void (mapErrorSM $ runStep (tsClient ts) $ AddTokens n)

buyTokens :: HasBlockchainActions s => TokenSale -> Integer -> Contract w s Text ()
buyTokens ts n = void $ mapErrorSM $ runStep (tsClient ts) $ BuyTokens n

withdraw :: HasBlockchainActions s => TokenSale -> Integer -> Integer -> Contract w s Text ()
withdraw ts n l = void $ mapErrorSM $ runStep (tsClient ts) $ Withdraw n l
```

- Create three schemas
  - First two for creation
  - Third for usage
```haskell
type TSStartSchema = BlockchainActions
    .\/ Endpoint "start"      (CurrencySymbol, TokenName)
type TSStartSchema' = BlockchainActions
    .\/ Endpoint "start"      (CurrencySymbol, CurrencySymbol, TokenName)
type TSUseSchema = BlockchainActions
    .\/ Endpoint "set price"  Integer
    .\/ Endpoint "add tokens" Integer
    .\/ Endpoint "buy tokens" Integer
    .\/ Endpoint "withdraw"   (Integer, Integer)
```

### Testing

- Trace
```haskell
myTrace :: EmulatorTrace ()
myTrace = do
    h <- activateContractWallet (Wallet 1) startEndpoint
    -- Create Token Sale
    callEndpoint @"start" h (currency, name)
    void $ Emulator.waitNSlots 5
    Last m <- observableState h
    -- Check that Token Sale was created
    case m of
        Nothing -> Extras.logError @String "error starting token sale"
        Just ts -> do
            Extras.logInfo $ "started token sale " ++ show ts

            h1 <- activateContractWallet (Wallet 1) $ useEndpoints ts
            h2 <- activateContractWallet (Wallet 2) $ useEndpoints ts
            h3 <- activateContractWallet (Wallet 3) $ useEndpoints ts

            -- Call endpoints
            callEndpoint @"set price" h1 1_000_000
            void $ Emulator.waitNSlots 5

            callEndpoint @"add tokens" h1 100
            void $ Emulator.waitNSlots 5

            callEndpoint @"buy tokens" h2 20
            void $ Emulator.waitNSlots 5

            callEndpoint @"buy tokens" h3 5
            void $ Emulator.waitNSlots 5

            callEndpoint @"withdraw" h1 (40, 10_000_000)
            void $ Emulator.waitNSlots 5
```



## Testing (with Tasty)

- [Tasty on Hackage](https://hackage.haskell.org/package/tasty)
  - Tests are of type `TestTree`
  - Allows to group and subgroup tests
- Support for tests in Plutus: `Plutus.Contract.Test`
- Two variants
  1. One with `EmulatorTrace`s
  2. More sophisticated ones: Use property based testing



### Emulator based tests

- `checkPredicate :: String -> TracePredicate -> EmulatorTrace () -> TestTree`
  - Takes descriptive name
  - Predicates `TracePredicate` which the test must fulfill
  - A trace as known, `EmulatorTrace ()`
  - Result `TestTree`, input for tasty
- Variant `checkPredicateOptions :: CheckOptions -> ...`
  - Allows to add `CheckOptions`
  - No constructors shown, must be accessed via lenses
    - Starting from `defaultCheckOptions :: CheckOptions`
  - Note: Optics and lenses are huge topic on its own
    - Highly recommended book (author, not Lars here): [Optics By Example by Chris Penner](https://leanpub.com/optics-by-example)

- `TracePredicate`
  - Logical combinators: `not`, `.&&.`
  - Multiple checks possible, e.g. `queryingUtxoAt`, `assertDone` (Contract has completed), ...
  - Here only one: Check funds with `walletFundsChange`
    - Note: Checks for a change in funds after execution
    - Fees are ignored
    - Variant which checks feeds: `walletFundsExactChange`

- Example
```haskell
tests :: TestTree
tests = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "token sale trace"
    (     walletFundsChange (Wallet 1) (Ada.lovelaceValueOf   10_000_000  <> assetClassValue token (-60))
     .&&. walletFundsChange (Wallet 2) (Ada.lovelaceValueOf (-20_000_000) <> assetClassValue token   20)
     .&&. walletFundsChange (Wallet 3) (Ada.lovelaceValueOf (- 5_000_000) <> assetClassValue token    5)
    )
    myTrace
```
- Run with `defaultMain tests` (e.g. in repl)
  - If successful: Just OK logged
  - Failure also shows what was expected and what was received
    - Along with trace
- In essence: Unit tests




## Optics (lens)

- [lens](https://hackage.haskell.org/package/lens)
  - Whole zoo of optics
  - Essence: Reaching deeply into data types and manipulate them

- Example data
  - Note: When using lenses, naming with underscore `_staff` is convention
  - Data type
```haskell
newtype Company = Company {_staff :: [Person]} deriving Show

data Person  = Person
    { _name    :: String
    , _address :: Address
    } deriving Show

newtype Address = Address {_city :: String} deriving Show

alejandro, lars :: Person
alejandro = Person
  {  _name    = "Alejandro"
  ,  _address = Address {_city = "Zacateca"}
  }
lars = Person
  {  _name    = "Lars"
  ,  _address = Address {_city = "Regensburg"}
  }

iohk :: Company
iohk = Company { _staff = [alejandro, lars] }
```

- Task: Write function which changes the cities of all the persons in the company to the new city name

- Traditional and rather ugly way
  - With record update syntax `c { _staff = ...}`
  - And `map` over list
```haskell
goTo :: String -> Company -> Company
goTo there c = c {_staff = map movePerson (_staff c)}
  where
    movePerson p = p {_address = (_address p) {_city = there}}
```

- Moving to optics: Create lenses automatically with Template Haskell
  - With `makeLenses`
  - Requires the underscore conventions
  - See what template haskell does in repl `:set -ddump-splices`
    - Requires code change
    - Creates `Iso`, `Lens'`
    - Often two types: The bigger one first, the "zooming in" as second
    - E.g. `staff :: Iso' Company [Person]`
      - Allows zooming in on the persons `[Person]` of a `Company`
- Example: `lars ^. address . city`
  - I.e. allows combining/chaining lenses
  - Or manipulate:
    - `lars & name .~ "LARS"`
    - `lars & address . city .~ "Munich"`
- Another type of optics: Traversables
  - Allows zooming in on multiple smaller parts simultaneously
  - Like each element of a list
  - E.g. set every element in a list to `42`: `[1 :: Int, 3, 4] & each .~ 42`

- Aim: Similar to OOP access of fields

- Impl of `goTo`
```haskell
makeLenses ''Company
makeLenses ''Person
makeLenses ''Address

goTo' :: String -> Company -> Company
goTo' there c = c & staff
                  . each
                  . address
                  . city
                  .~ there
```

- Closing circle: Setting check options
  - `defaultCheckOptions & emulatorConfig .~ emCfg`
  - Sets the `emulatorConfig` in the `defaultCheckOptions` to `emCfg`


## Property-based testing (with QuickCheck)

- [QuickCheck](https://hackage.haskell.org/package/QuickCheck)
  - Revolutionary idea about testing
  - First introduced into Haskell: Good fit because of pureness
  - But copied by all other languages already
  - Invented by John Hughes (also one of inventor of Haskell)
    - Helped with his company to provide support for Plutus

- Simple usage
  - Unit test just a special case
```haskell
prop_simple :: Bool
prop_simple = 2 + 2 == (4 :: Int)
```
  - Test with `quickCheck prop_simple`
  - Checks if property holds

- More interesting example: Insertion sort
```haskell
sort :: [Int] -> [Int] -- not correct
sort []     =  []
sort (x:xs) =  insert x xs

insert :: Int -> [Int] -> [Int] -- not correct
insert x []                     =  [x]
insert x (y:ys)  | x <= y       =  x : ys
                 | otherwise    =  y : insert x ys

```
  - Test e.g. property is to check if sorted after inserting an element
```haskell
isSorted :: [Int] -> Bool
isSorted []           = True
isSorted [_]          = True
isSorted (x : y : ys) = x <= y && isSorted (y : ys)
```
  - Property: Takes `[Int]` and returns a bool
    - Like a specification
    - For all list of `Int`s, if `sort` is applied
    - the list should be sorted
```haskell
prop_sort_sorts :: [Int] -> Bool
prop_sort_sorts xs = isSorted $ sort xs
```
  - Test with `quickCheck prop_sort_sorts`
    - Fails, provides counter example: `[0,0,-1]`
    - Testing it: `sort [0,0,-1]`
    - Result is `[0,-1]`
      - Not sorted
      - Lost one element

- How it works
  - Quick check generates random input list of arguments
  - Here: List of ints, `[Int]`
  - For each inputs: Check if properties hold
  - Even more clever:
    - Quick check states: `Failed! Falsified (after 8 tests and 4 shrinks)`
    - Does not only find counter example (after 8 tests)
    - But simplified counter example in 4 shrinks
      - Like dropping elements of counter example
    - Helps since smaller failuer input data better for debugging

- Random input data can be seen with `sample (arbitrary :: Gen [Int])`
  - Prints out some example input data
    - Note: The later the example, the more complex the example is
  - Generation done by using type class `Arbitrary`
    ```haskell
    class Arbitrary a where
      arbitrary :: Gen a
      shrink :: a -> [a]
    ```
    - `Gen` is a monad
      - Also has a notion of complexity (see sample output)
      - QuickCheck starts with simple input and progresses to more complex inputs
    - Since there is a `Arbitrary Int`, and a `Arbitrary [a]`, `Arbitrary [Int]` can be derived
  - Lot of instances for this type class


- Fixing sort
```haskell
sort :: [Int] -> [Int]
sort []     =  []
sort (x:xs) =  insert x $ sort xs
```
  - Quick Check does not fail anymore
  - But counter example, `[0,0,-1]` still wrong

- Insight: Need another property
  - Add length property
```haskell
prop_sort_preserves_length :: [Int] -> Bool
prop_sort_preserves_length xs = length (sort xs) == length xs
```

- Fix insert (missing `y`)
```haskell
insert :: Int -> [Int] -> [Int]
insert x []                     =  [x]
insert x (y:ys)  | x <= y       =  x : y : ys
                 | otherwise    =  y : insert x ys
```

- But: Still properties not sufficient
  - E.g. sort function could just return list with all `0` of same length
  - Tests can only be as good as properties are
- But in general: Allows to test a lot more cases than with unit tests
  - Especially considers "corner" cases like `[]`, negative numbers, ...
  - Per default: Quick Check runs only 100 tests
  - For real test suite: Should be increased to 1000 or even 10_000 (test server)


## Testing Plutus

- Issue: How to Quick Check code with side effects
  - Arises with all systems which use IO
  - Quick Check solution shown with file system IO
- Plutus similar solution to file system IO

- Approach
  - Build a model of a real system; have a relation
  - Generate a sequence of actions
  - Apply action to real system and to model
  - Compare results
  - Repeat with next action
  - Shrinking equals dropping actions and check if bug is still present

```
┌─────────┐         ┌─────────┐
│         │ Action  │         │ Action
│         │         │         │
│ System  ├────────►│ System  ├────────► ── ──
│         │         │         │
│         │         │         │
└────▲────┘         └────▲────┘
     │                   │
     │                   │
     │                   │
     │                   │ ?
     │                   │
     │                   │
     │                   │
     │                   │
┌────▼────┐         ┌────▼────┐
│         │ Action  │         │ Action
│         │         │         │
│  Model  ├────────►│  Model  ├───────► ── ──
│         │         │         │
│         │         │         │
└─────────┘         └─────────┘
```

- For Plutus
  - Come up with model and define expectations
    - Like how endpoints should change the model
  - Provide a link between model and real system (or here: Emulator)
  - Apply Quick Check machinery

### Application to Token Sale

- Import `Plutus.Contract.Test` and `Plutus.Contract.Test.ContractModel` (for Quick Check testing)
  - As well as `Test.QuickCheck`, `Test.Tasty`, `Test.Tasty.QuickCheck`

- Prepare model
  - `TSState` represents state of one Token Sale instance
  - Model:
    - Idea is each wallet runs it's own Token Sale contract
    - Will trade different tokens
    - Initial state is empty map, so no contract started for a wallet
```haskell
data TSState = TSState
    { _tssPrice    :: !Integer
    , _tssLovelace :: !Integer
    , _tssToken    :: !Integer
    } deriving Show

newtype TSModel = TSModel {_tsModel :: Map Wallet TSState}
    deriving Show
```

- Create instance `ContractModel TSModel`: Contains all logic
  - How model should behave
  - And how model is linked to actual system
  - General requirements:
    - Define actions `data Action TSModel` which Quick Check can generate
    - Define GADT which provides a key for each instance
      - Required for two contract types: Start and Use
    - Define `instanceTag`: Returns different tag for each running contract instance
    - Define `arbitraryAction`: Generate an action from possible actions
      - Try out in repl with `sample (arbitraryAction undefined :: Gen (Action TSModel))`
    - Define `initialState`: Model with empty map in our case
    - Define `nextState` for each action
      - So in essence: What effect an action has on the model
      - `nextState :: ContractModel state => Action -> Spec state ()`
        - `Spec` monad
        - Allows to inspect current state of model and
        - transfer funds in model

```haskell
instance ContractModel TSModel where

    -- Actions which Quick Check can generate:
    -- - I.e. one constructor for each endpoint.
    -- - Additional arguments since it must kept track which wallets does which action
    data Action TSModel =
              -- Wallet starts Token Sale
              Start Wallet
              -- 2nd wallet sets price of Token Sale of first wallet
              -- Note: Should only work if both wallets are the same.
              --   So cover this allows testing this expectation.
            | SetPrice Wallet Wallet Integer
              -- Same here with wallet order: 2nd adds tokens to Token Sale operated by 1st wallet
            | AddTokens Wallet Wallet Integer
            | Withdraw Wallet Wallet Integer Integer
            | BuyTokens Wallet Wallet Integer
        deriving (Show, Eq)

    -- For each contract which is running, define a key.
    -- Two different contract types: Start and Use.
    -- Note: GADT required, since return type is different for different constructors
    data ContractInstanceKey TSModel w s e where
        -- Wallet which starts a contract
        -- Use version which explicitly passes in NFT
        StartKey :: Wallet           -> ContractInstanceKey TSModel (Last TokenSale) TSStartSchema' Text
        -- First owns Token Sale, second wallet interacts.
        UseKey   :: Wallet -> Wallet -> ContractInstanceKey TSModel ()               TSUseSchema    Text

    -- Required: Key already defines wallet which runs the contract; so second argument can be ignored.
    -- Important: Must result in different tag for each running instance
    instanceTag key _ = fromString $ "instance tag for: " ++ show key

    -- Define how, from the Actions, an arbitrary action can be generated
    -- Note: Argument is model state which is not needed
    -- - `oneof` is combinator provided by Quick Check: Randomly pics one action out of a list of actions
    -- - Uses `genWallet` which generates a wallet: uses `elements wallets` to pic generate wallets
    -- - By using Applicative application 'Start <$> genWallet', generates an action out of it
    -- - Note: `genWallet :: Gen Wallet`, `(Start <$> genWallet) :: Gen Action`
    arbitraryAction _ = oneof $
        (Start <$> genWallet) :
        -- `SetPrice` requires two wallets, and a non negative number (generated by `genNonNeg`)
        [ SetPrice  <$> genWallet <*> genWallet <*> genNonNeg ]               ++
        [ AddTokens <$> genWallet <*> genWallet <*> genNonNeg ]               ++
        [ BuyTokens <$> genWallet <*> genWallet <*> genNonNeg ]               ++
        [ Withdraw  <$> genWallet <*> genWallet <*> genNonNeg <*> genNonNeg ]

    initialState = TSModel Map.empty

    -- Effect on model if Start action happens
    nextState (Start w) = do
        -- `withdraw` is from `Spec` monad: Means some funds go away from a wallet (does not matter where to)
        -- Here: Wallet `w` looses NFT
        withdraw w $ nfts Map.! w
        -- `$=` from `Spec` monad: Lens on left handside, right hand side: model change
        (tsModel . at w) $= Just (TSState 0 0 0)
        -- Also from `Spec` monad. So knows about time, i.e. start will take 1 slot.
        wait 1

    nextState (SetPrice v w p) = do
        -- Check if wallets are same, only then do something
        when (v == w) $
            -- Update model to new price
            -- Note: No funds change
            (tsModel . ix v . tssPrice) $= p
        wait 1

    nextState (AddTokens v w n) = do
        -- Helper function hasStarted:
        -- - Defined in `Spec` monad
        -- - Uses `getModelState` of `Spec` monad to get current state
        -- - Tries to extract `TSState`
        -- - Returns true if successful
        started <- hasStarted v
        when (n > 0 && started) $ do
            -- Extract from model state the balance change
            -- Used to check if wallet w has enough funds
            bc <- askModelState $ view $ balanceChange w
            let token = tokens Map.! v
            -- Check if enough funds
            when (tokenAmt + assetClassValueOf bc token >= n) $ do  -- does the wallet have the tokens to give?
                -- Wallet will loose tokens to contract
                withdraw w $ assetClassValue token n
                -- State of contract changes
                -- Using `$~` from `Spec`: Applies function to current value
                (tsModel . ix v . tssToken) $~ (+ n)
        wait 1

    nextState (BuyTokens v w n) = do
        when (n > 0) $ do
            -- Get current state of started Token Sale
            m <- getTSState v
            case m of
                -- Check if there are enough tokens which buyer wants to buy
                Just t
                    | t ^. tssToken >= n -> do
                        let p = t ^. tssPrice
                            l = p * n
                        -- Buyer looses money
                        withdraw w $ lovelaceValueOf l
                        -- Gains tokens, opposite of `withdraw`
                        deposit w $ assetClassValue (tokens Map.! v) n
                        -- Update state of contract: Lovelace and tokens
                        (tsModel . ix v . tssLovelace) $~ (+ l)
                        (tsModel . ix v . tssToken)    $~ (+ (- n))
                _ -> return ()
        wait 1

    nextState (Withdraw v w n l) = do
        when (v == w) $ do
            m <- getTSState v
            case m of
                Just t
                    | t ^. tssToken >= n && t ^. tssLovelace >= l -> do
                        -- Gain to owner regarding lovelace and tokens
                        deposit w $ lovelaceValueOf l <> assetClassValue (tokens Map.! w) n
                        -- Update state of Token Sale
                        (tsModel . ix v . tssLovelace) $~ (+ (- l))
                        (tsModel . ix v . tssToken) $~ (+ (- n))
                _ -> return ()
        wait 1

    -- So far:
    -- - No link between model and actual contracts
    -- - Despite names are like `Redeemer`
    -- Type: `perform :: ContractModel state => HandlerFun state -> ModelState state -> Action state -> Plutus.Trace.Emulator.EmulatorTrace ()`
    -- - `HandlerFun` gives access to actual contract handles
    -- - Pattern match of actions defined here, to Emulator code
    -- - Call endpoint "start"
    --   - Get handle on instance with `h` (second argument to `calEndpoint`)
    --   - `h` expects a key which defined in `ContractInstanceKey`
    -- - Add a delay after the action with `delay 1`
    perform h _ cmd = case cmd of
        (Start w)          -> callEndpoint @"start"      (h $ StartKey w) (nftCurrencies Map.! w, tokenCurrencies Map.! w, tokenNames Map.! w) >> delay 1
        (SetPrice v w p)   -> callEndpoint @"set price"  (h $ UseKey v w) p                                                                    >> delay 1
        (AddTokens v w n)  -> callEndpoint @"add tokens" (h $ UseKey v w) n                                                                    >> delay 1
        (BuyTokens v w n)  -> callEndpoint @"buy tokens" (h $ UseKey v w) n                                                                    >> delay 1
        (Withdraw v w n l) -> callEndpoint @"withdraw"   (h $ UseKey v w) (n, l)                                                               >> delay 1

    -- Define preconditions: I.e. conditions under which an action is allowed to happen
    -- For Start: Token Sale has not yet started
    precondition s (Start w)          = isNothing $ getTSState' s w
    -- All others: Token Sale must have been started
    precondition s (SetPrice v _ _)   = isJust    $ getTSState' s v
    precondition s (AddTokens v _ _)  = isJust    $ getTSState' s v
    precondition s (BuyTokens v _ _)  = isJust    $ getTSState' s v
    precondition s (Withdraw v _ _ _) = isJust    $ getTSState' s v
```

- Link keys to actual contracts with `instanceSpec`
  - `ContractInstanceSpec` takes
    - Key, Wallet, actual contract
  - For example
    - For each wallet we start a Token Sale, `StartKey`
    - For all pairs of wallets: Run `UseKey` with the correct Token Sale
```haskell
instanceSpec :: [ContractInstanceSpec TSModel]
instanceSpec =
    [ContractInstanceSpec (StartKey w) w startEndpoint' | w <- wallets] ++
    [ContractInstanceSpec (UseKey v w) w $ useEndpoints $ tss Map.! v | v <- wallets, w <- wallets]
```


- Finally: Define Quick Check property
  - Link everything together
  - With `propRunActionsWithOptions`
```haskell
propRunActionsWithOptions
  :: ContractModel state
  -- Setup initial distribution etc. .
  => Plutus.Contract.Test.CheckOptions
  -- == `instanceSpec`
  -> [ContractInstanceSpec state]
  -- Allows inserting additional tests
  -> (ModelState state -> Plutus.Contract.Test.TracePredicate)
  -- List of actions
  -> Actions state
  -- Quick Check property
  -> Property
```
  - Property
    - Try out generating `Actions state` with
      `sample (arbitrary :: Gen (Actions TSModel))`
```haskell
prop_TS :: Actions TSModel -> Property
prop_TS = withMaxSuccess 100 . propRunActionsWithOptions
    -- Setup initial distribution
    (defaultCheckOptions & emulatorConfig .~ EmulatorConfig (Left d))
    instanceSpec
    -- No additional tests
    (const $ pure True)
    -- `Actions state` is generated by Quick Check
  where
    d :: InitialDistribution
    d = Map.fromList $ [ ( w
                         , lovelaceValueOf 1000_000_000 <>
                           (nfts Map.! w)               <>
                           mconcat [assetClassValue t tokenAmt | t <- Map.elems tokens])
                       | w <- wallets
                       ]
```

- What is tested?
  - For all sequences of actions
  - The flow defined in the model (how funds flow etc.)
  - Corresponds to what actually happens in the Emulator

- Will take some time to run

- Test failing by e.g. allowing anybody to call `SetPrice`
  - Quick Check finds bug
  - Reports action seqeuence to reproduce
```
Actions
 [Start (Wallet 2),
  SetPrice (Wallet 2) (Wallet 1) 3,
  AddTokens (Wallet 2) (Wallet 1) 9,
  BuyTokens (Wallet 2) (Wallet 2) 3]
Expected funds of W2 to change by
  Value (Map [(02,Map [("NFT",-1)]),(bb,Map [("B",3)])])
  (excluding 14869 lovelace in fees)
but they changed by
  Value (Map [(,Map [("",-9)]),(02,Map [("NFT",-1)]),(bb,Map [("B",3)])])
```
  - `SetPrice` was called by wallet 1 and succeeds, so Emulator sets token price to 12
  - But in model: Specification states that no change should happen if wallets are different for `SetPrice`
  - => Failure due to mismatch in Emulator and Model

- Integration with tasty
```haskell
tests :: TestTree
tests = testProperty "token sale model" prop_TS
```

- Add a `test-suite` to cabal file (similar to `library` and `executable`)
  - Provide a `Spec.hs`: Imports tests and executes them with `defaultMain`
  - Run with `cabal test`



## Limitations of Property-based Testing

- Tests only contracts which are provided
  - So not all possible off-chain code
  - So bad actor can write own off-chain code which allows stealing funds
- Concurrency can happen on blockchain
  - Not nicely sequenced with our waits
  - In order to handle this in test case: Must express what happens
  - Hard since heavily relies on order of execution



## Homework

- Modify `TokenSale`
- Allow additional transition `Close`
  - Only called by seller
  - Collect all remaining tokens, Lovelaces and NFT
- Requires to modify state, use `State (Maybe Integer)`
  - `Nothing` means closed
- Modify trace and model for additional operation
  - Model: New action close
  - Trace: Final step is close
