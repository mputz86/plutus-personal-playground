---
date: 2021-04-27T14:36
---

# Cardano Plutus Pioneer Program: Lecture 4

## Review

- On-chain part
- Validation logic
- Compiled to plutus script
- Lives on blockchain
- Executed by nodes which validate a transaction

- There is more
  - More complex examples
  - Which may do more sophisticated use of context
  - Like looking at inputs and outputs
  - Or NFTs
  - I.e. Plutus can be used to mint and burn native tokens

## Off-chain part

- Built transaction which will be validated on-chain
- On-chain: Validator in principle just plain function
  - Beside need to compile to Plutus Core via Template Haskell
  - If accepted, nothing special (no fancy Haskell stuff)
  - Only need:
    - All functions must be contained within the QuasiQuotes for template Haskell
    - Achieved by using `INLINABLE` pragma
    - Requires own `Prelude` where basic functions are labled with this pragma
    - I.e. usable in validator
- Off-chain: Plain Haskell, uses sophisticated Haskell stuff
  - Monads, Streaming

## IO

- In Java no clue what happens between two function calls
  - And therefore not clear, if the same value will be returned
  - Makes testing harder
  - I.e. it is not possible to replace the function call everywhere with just the result
- Haskell different: Has **referential transparency**
  - I.e. if you have a function
      ```haskell
      foo :: Int
      foo = ...
      ```
  - Than it is possible to replace any call to this function with the return value
  - Makes refactoring very easy
  - Testing is easier: Just check that the right value is returned
- But: In order to interact with the world, side-effects are needed
  - Like: reading from terminal, print on screen, read from file, write to file, read from network
  - Recommended video: [Simon Peyton Jones, Haskell is Useless](https://www.youtube.com/watch?v=iSmkqocn0oQ) (6:22min)

- In Haskell: IO Monad
  ```haskell
  foo :: IO Int
  foo = ...
  ```
  - `IO` is type constructor (like `Maybe` or `List`)
  - `IO` special: You cannot implement it in the language, i.e. built in primitive
  - `IO Int` means:
    - Computation, which computes an `Int`
    - But may contain side-effects
    - Note: Referential transparency is **not** broken
      - `IO Int` is just a recipe
      - If evaluated, the recipe is not executed
      - Result of computation is just a recipe of getting an `Int`
      - No execution is happening
    - Only way to execute is in `main` (executable, main entry point)
      - Or in repl, `ghci`
- Main in Haskell
  ```haskell
  main :: IO ()
  main = putStrLn "Hello, world!"
  ```
  - Returns nothing, unit, `()`
  - Type of `putStrLn :: String -> IO ()`
  - Note: Add a `executable` part in `cabal` file
    - Filename can be arbitrary
    - Exception to normal Haskell: Module names must be named like filenames

## Combining IO actions

- `Functor` type class
  - Important function: `fmap :: (a -> b) -> f a -> f b`
  - Here: `f` is `IO`
  - Example:
    - Import `toUpper :: Char -> Char` from `Data.Char`
    - Note: String is defined as list of `Char`, `type String = [Char]`
    - Use `map` to convert a String to upper case: `map toUpper`
      - here it's: `map :: (a -> b) -> [a] -> [b]`
    - Use `fmap` to convert input to upper case:
      `fmap (map toUpper) getLine :: IO [Char]`

- Chaining `IO` actions together, ignoring result of previous action
  - With sequence operator `>>`
  - Usage: `putStrLn "Hello" >> putStrLn "World"`

- Chaining without ignoring result of previous
  - Bind operator: `>>= :: Monad m => m a -> (a -> m b) -> m b`
  - Usage: `getLine >>= putStrLn`
- Function which returns value immediately without side-effects
  - `return :: Monad m => a -> m a`

- Example: Read two inputs, print them
  ```haskell
  main :: IO ()
  main = bar

  bar :: IO ()
  bar = getLine >>= \s ->
        getLine >>= \t ->
        putStrLn (s ++ t)
  ```

## Maybe

- Definition
  ```haskell
  data Maybe a = Nothing | Just a
  ```
  - Optional value
- Example:
  - `import Text.Read (readMaybe)`
  - `read :: String -> a`
    - Parse String to value (which has a `Read` instance): Use `read`
    - `read "42" :: Int`
    - Problem: `read "ab" :: Int`, get error
    - Better: `readMaybe :: String -> Maybe Int`
    - Returns `Nothing` if reading fails
- Using `readMaybe`, example
  ```haskell
  foo :: String -> String -> String -> Maybe Int
  foo x y z = readMaybe x of
    Nothing -> Nothing
    Just k  -> case readMaybe y of
      Nothing -> Nothing
      Just l  -> case readMaybe z of
        Nothing -> Nothing
        Just m  -> Just (k + l + m)
  ```
  - Testing
  ```haskell
  > readMaybe "1" "2" "3"
  Just 6
  > readMaybe "" "2" "3"
  Nothing
  ```
- Problem: Code repeats a lot
  - Create a helper function
    ```haskell
    bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
    bindMaybe Nothing _ = Nothing
    bindMaybe (Just x) f = f x

    foo' :: String -> String -> String -> Maybe Int
    foo' x y z = readMaybe x `bindMaybe` \k ->
                 readMaybe y `bindMaybe` \l ->
                 readMaybe z `bindMaybe` \m ->
                 Just (k + l + m)
    ```
  - No dangling case function
  - More compact, business logic clearer, less noise
  - Kind of like exceptions: Stop as soon as `Nothing` is encountered

## Either

- Issue with `Nothing`: No error message
- Use `Either`
  ```haskell
  data Either a b = Left a | Right b
  ```
- Usage
  ```haskell
  readEither :: Read a => String -> Either String a
  readEither s = case readMaybe s of
    Nothing -> Left $ "Can't parse: " ++ s
    Just a  -> Right a
  ```
  - Use in `foo`: replace `readMaybe` with `readEither` (dangling impl)
- Fixed by rewriting `bindMaybe` to `bindEither` and using it in `foo'`


## Writer

- For log messages
  ```haskell
  data Writer a = Writer a [String]
    deriving Show

  number :: Int -> Writer Int
  number n = Writer n $ ["number: " ++ show n]

  foo :: Writer Int -> Writer Int -> Writer Int -> Writer Int
  foo (Writer k xs) (Writer l ys) (Writer m zs) =
    Writer (k + l + m) $ xs ++ ys ++ zs
  ```
- Introducing `tell`
  ```haskell
  tell :: [String] -> Writer ()
  tell = Writer ()

  foo :: Writer Int -> Writer Int -> Writer Int -> Writer Int
  foo (Writer k xs) (Writer l ys) (Writer m zs) =
    let s = k + l + m
        Writer  _ us = tell ["Sum: " ++ show s]
     in Writer s $ xs ++ ys ++ zs ++ us
  ```

- `bindWriter`:
  ```haskell
  bindWriter :: Writer a -> (a -> Writer b) -> Writer b`
  bindWriter (Writer a xs) f =
    let Writer b ys = f a
     in Writer b $ xs ++ ys

  foo' :: ...
  foo' x y z = x `bindWriter` \k ->
               y `bindWriter` \l ->
               z `bindWriter` \m ->
               let s = k + l + m
                in tell ["Sum: " ++ show s] `bindWriter` \_ ->
                     Writer s []
  ```
  - No longer required to concat log messages
    - Could be forgotten
    - Order wrong
  - No pattern matching requird on input params
  - Concentrage more on business logic

- Compared to `Maybe` and `Either`:
  - Not error handling addressed here but logging
  - Sequencing of computations has different logic
  - For `Writer` it is combining log output


## Monad

- Looking at what `IO`, `Maybe`, `Either` and `Writer` had in common:
  - Binding operations
    ```haskell
    (>>=)      :: IO a            -> (a -> IO b)            -> IO b
    bindMaybe  :: Maybe a         -> (a -> Maybe b)         -> Maybe b
    bindEither :: Either String a -> (a -> Either String b) -> Either String b
    bindWriter :: Writer a        -> (a -> Writer b)        -> Writer b
    ```
  - => Main idea of Monads:
    - Concept of computation
    - With some additional side-effect
      - Side-effect can be real world, error, log outputs, ...
    - And possibility to bind two operations together
      - How combination works, depends on computation
  - Also always possible: Computation without side-effects
    ```haskell
    return              :: a -> IO a
    Just                :: a -> Maybe a
    Right               :: a -> Either String a
    (\a -> Writer a []) :: a -> Writer a
    ```
- These two operations define a Monad
  ```haskell
  class Applicative => Monad m where
    -- Bind
    (>>=) :: m a -> (a -> m b) -> m b
    -- (Helper function for bind)
    (>>) :: m a -> m b -> m b
    -- Computation without side-effect
    return :: a -> m a
  ```
- Note `Applicative`
  ```haskell
  class Functor f => Applicative f where
    -- Equals to `return`
    pure :: a -> f a
    -- Can be derived if a Monad is given, called 'ap'
    (<*>) :: f (a -> b) -> f a -> f b
    -- ...

  class Functor f where
    -- Given monad, can be derived
    fmap :: (a -> b) -> f a -> f b
    -- ...
  ```
- Example
  ```haskell
  import Control.Monad

  instance Functor Writer where
    fmap = liftM

  instance Applicative Writer where
    pure  = return
    (<*>) = ap

  instance Monad Writer where
    return a = Writer a []
    (>>=) = bindWriter
  ```

- Advantages
  - Important to identify common patterns and give them a name (i.e. Monad)
  - By using type class, common function names
    - No more `bindMaybe`, ...
    - Just `(>>=)`
  - Lot's of function provided which work for all Monads

- Example for a general Monad function
  ```haskell
  threeInts :: Monad m => m Int -> m Int -> m Int -> m Int
  threeInts mx my mz =
    mx >>= \k ->
    my >>= \l ->
    mz >>= \m ->
    let s = k + l + m
     in return s
  ```
  - Can be used for `foo'` in `Maybe`, `Either`, ...

- What is a Monad?
  - A computation with some sort of site-effect
  - Some special feature, special power
  - Each Monad instance it's own power
    - `IO` real world side-effects
    - `Maybe` to fail
    - `Either` to fail with error message
    - `Either [String]` log string messages
  - Required to provide a method which does not use the super power, `return`
    - `IO` without an real world side-effect
    - Potentially failing computation which does not fail
    - Logging without logging anything
  - And function to chain two computations together, `>>=` (bind)
  - Bind is like "an overloaded `;`"
    - But in Haskell: Can say what meaning of `;` is

- Since this is so common, `do`-Notation is used
  - The following
    ```haskell
    threeInts mx my mz =
      mx >>= \k ->
      my >>= \l ->
      mz >>= \m ->
      return $ k + l + m
    ```
  - can be written like this and has exactly the same meaning:
    ```haskell
    threeInts mx my mz = do
      k <- mx
      l <- my
      m <- mz
      return $ k + l + m
    ```
  - Note:
    - `let` can be used within a `do` block and does not require the `in` word
      ```haskell
      threeInts mx my mz = do
        k <- mx
        l <- my
        m <- mz
        let s = k + l + m
        return s
      ```
    - If not intersted in result of computation (i.e. `>>`), leave out the the `x <-` part
      ```haskell
      foo'' x y z = do
        s <- threeInts x y z
        tell ["Sum: " ++ show s]
        return s
      ```
  - In general
    - For longer computation use do notation
    - For shorter: Matter of taste


## Combining different monads

- Like need of multiple monads, e.g. logging and failure
  - One approach: Monad transformers, short `mtl`
    - Build custom monads which have these features
  - Other: Effect systems
    - Used in Plutus
    - E.g. `Contract w s e a` monad which runs in a wallet
    - `EmulatorTrace a` monad for testing
      - In principle what can be done in playground simulation

- Note: Do not have to understand effect systems in order to work with them
  - Just know that it is a monad
  - And what special power it has
  - Sufficient to know how monads work in general
  - Know for specific monad, what it can do

## `EmulatorTrace` Monad

- `runEmulatorTrace :: `
  - Executes trace on emulated block chain
  - Requires the trace to execute
  - Expects `EmulatorConfig` which allows to define initial chain state
    - I.e. `Value` in wallets at start
    - `Value` is not only ADA but can be native tokens
    - There is a `defaultDist`, which every wallet 100 ADA
  - Returns
    - List of emulator events
    - `Maybe EmulatorError`
    - State of emulator

- Test in repl
  ```haskell
  > import Plutus.Trace.Emulator
  > import Plutus.Contract.Trace
  > :t EmulatorConfig $ Left defaultDist
  EmulatorConfig $ Left defaultDist
  > runEmulatorTrace (EmulatorConfig $ Left defaultDist) $ return ()
  ...
  ```
  - `runEmulatorTrace` returns a lot of data, not useful for testing in repl
  - Prefer `runEmulatorTraceIO`
    - Does not require config, uses default
    - Prints out nicely
  - More control via `runEmulatorTraceIO'`
    - Allows to provide a `TraceConfig` for control on logging

- Example of vesting trace
  ```haskell
  test :: IO ()
  test = runEmulatorTraceIO myTrace

  myTrace :: EmulatorTrace ()
  myTrace = do
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"give" h1 $ GiveParmas
      { gpBeneficiary = pubKeyHash  . walletPubKey . Wallet $ 2
      , gpDeadline    = Slot 20
      , gpAmount      = 1000
      }
    void $ waitUntilSlot 20
    callEndpoint @"grab" h2 ()
    s <- waitNSlots 1
    Extras.logInfo $ "reached slot " ++ show s
  ```
  - Allows
    - Calling endpoints with params
    - Waiting for slots
    - Logging


## `Contract` Monad

- Off-chain code which runs in wallet
- Type parameters
  - `a` overall result of computation
  - `w` logging, like in writer monad
  - `s` blockchain specific capabilities
    - Like waiting slots
    - Waiting for transaction
    - Finding out own private key
    - Handling specific endpoints
  - `e` blockchain specific capabilities
    - Types of error messages

- Simple Example
  ```haskell
  myContract1 :: Contract () BlockchainActions Text ()
  myContract1 = Contract.logInfo @Text "Hello from the contract"

  myTrace1 :: EmulatorTrace ()
  myTrace1 = void $ activateContractWallet (Wallet 1) myContract1

  test1 :: IO ()
  test1 = runEmulatorTraceIO myTrace1
  ```
  - Note: Need to add the `@Text` since language extension `OverloadedStrings` is used
    - which allows that `""` are not only `String` but maybe of other types, like `Text`
    - `@Text` is a type application (requires language extension `TypeApplications`) and so compiler knows what type this `"Hello from the contract"` has

- Error Example
  ```haskell
  myContract1 :: Contract () BlockchainActions Text ()
  myContract1 = do
    void $ Contract.throwError "Boom!"
    Contract.logInfo @Text "Hello from the contract"
  -- ...
  ```

- Handling Error Example
  ```haskell
  myContract2 :: Contract () BlockchainActions Void ()
  myContract2 = Contract.handleError
    (\err -> Contract.logError $ "Cought error: " ++ unpack err)
    myContract1
  ```

- Look at `BlockchainActions`
  - Needs to be a different type if endpoints should be used
  - Done by extending type
    ```haskell
    type MySchema = BlockchainActions
        .\/ Endpoint "foo" Int

    myContract3 :: Contract () MySchema Text ()
    myContract3 = do
      n <- endpoint @"foo"
      Contract.logInfo n

    myTrace3 :: EmulatorTrace ()
    myTrace3 = do
      h <- activateContractWallet (Wallet 1) myContract3
      callEndpoint @"foo" h 42
    ```
  - Requires language extensions `DataKinds` and `TypeOperations`
  - `@"foo"` is a type level string
  - Important: In `Contract`
    - `endpoint @"foo"` means that this endpoint is called from the outside and provides and `Int`
    - Contract will block until endpoint was called


- Looking at `w`, logging
  - `w` must be a `Monoid`
    - `Monoid a` has functions
      - `mempty :: a` for an empty value and
      - `mappend :: a -> a -> a` for appending values together function

  - Example with `w` being `[Int]`
    ```haskell
    myContract4 :: Contract [Int] BlockchainActions Text ()
    myContract4 = do
      void $ Contract.waitNSlots 10
      tell [1]
      void $ Contract.waitNSlots 10
      tell [2]
      void $ Contract.waitNSlots 10

    myTrace4 :: EmulatorTrace ()
    myTrace4 = do
      h <- activateContractWallet (Wallet 1) myContract4

      void $ Emulator.waitNSlots 5
      xs <- observableState h
      Extras.logInfo $ show xs

      void $ Emulator.waitNSlots 10
      ys <- observableState h
      Extras.logInfo $ show ys

      void $ Emulator.waitNSlots 10
      zs <- observableState h
      Extras.logInfo $ show zs
    ```
  - Helps to communicate from `Contract`
    - To `Emulator`, but also to
    - PAB (Plutus application backend), covered later

- Bi-directional communication
  - Into `Contract` via endpoints
  - Out of `Contract` via `tell` mechanism



## Homework

- `submitTx` will find a suitable output to balance the transation (in `Contract`)
- `payContract` contract calls itself in order to be able to call endpoint as often as desired

- Task 1
  - Write `EmulatorTrace` which takes to `Integer` as params
  - It should run `payContract` twice on wallet 1 and sent money to wallet 2
  - `Integer` Parameters are the amounts of money to send

- Note:
  - Test1 will succeed
  - Test2 will fail because
    - of not enough funds for first payment
    - and seconds payment will never happen


- Task 2
  - Modify pay contract to handle error
  - It should log error but not crash
  - Second payment should succeed

