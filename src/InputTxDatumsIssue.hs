{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module InputTxDatumsIssue where

import Control.Lens
import Data.Text.Prettyprint.Doc (Pretty (..), defaultLayoutOptions, layoutPretty)
import Data.Text.Prettyprint.Doc.Render.String (renderString)
import Control.Monad hiding (fmap)
import qualified Control.Monad.Freer.Extras as Extras
import Data.Aeson (FromJSON, ToJSON)
import Data.Map as Map
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.AddressMap (UtxoMap)
import Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Plutus.Contract hiding (when)
import qualified Plutus.Trace.Emulator as Emulator
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import qualified Wallet.Emulator.Wallet as Wallet
import Prelude (Semigroup (..))
import qualified Prelude as Haskell
import Wallet.Emulator
import Wallet.Emulator.MultiAgent
import Plutus.Trace.Emulator.Types
import qualified Data.Aeson as A
import Data.Text.Prettyprint.Doc.Extras
import Plutus.Trace
import Data.Default ( Default(def) )
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 (unpack)

-- | Play around with tx input datum in validator / on-chain.
--   Especially the 'countState' function is of interest.
--
-- Solution:
-- - Requires to add 'mustIncludeDatum' in the constraints
-- - Datum is added to 'txInfoData' and can be used via 'findDatum'
--
-- - But: According to Lars, input datum of all spent UTxOs should be available "per default"
--
data InputTxDatumsState = StateA | StateB | Final
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''InputTxDatumsState

{-# INLINEABLE countState #-}
countState :: (InputTxDatumsState -> Bool) -> TxInfo -> [TxInInfo] -> Integer
countState countStateF txInfo = go
  where
    go [] = 0
    go (TxInInfo {txInInfoResolved = txOut} : is) = fromMaybe (go is) $ do
      h <- txOutDatumHash txOut
      -- Issue: Cannot find datum for input
      --   Solved by providing Datum via 'mustIncludeDatum' constraints

      -- 1. Working: Just check if there is a hash
      -- Just $ 1 + go is

      -- 2. Not Working: Check if findDatum finds datum for hash
      --
      -- Datum d <- findDatum h txInfo
      -- Just $ 1 + go is

      -- 3. Not working, desired to check on state
      Datum d <- findDatum h txInfo
      s <- PlutusTx.fromData @InputTxDatumsState d
      if countStateF s
         then Just $ 1 + go is
         else Nothing

{-# INLINEABLE mkValidator #-}
mkValidator :: InputTxDatumsState -> () -> ScriptContext -> Bool
mkValidator d _ ScriptContext {scriptContextTxInfo = txInfo} =
  -- Checks that tx is spending all UTxO: I.e. one with StateA, one with StateB
  traceIfFalse
    "Not matchin state input UTxOs"
    (2 == countState f txInfo (txInfoInputs txInfo))
  where
    f StateA = True
    f StateB = True
    f _ = False

data InputTxDatums

instance Scripts.ScriptType InputTxDatums where
  type DatumType InputTxDatums = InputTxDatumsState
  type RedeemerType InputTxDatums = ()

inst :: Scripts.ScriptInstance InputTxDatums
inst =
  Scripts.validator @InputTxDatums
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @InputTxDatumsState @()

validator :: Validator
validator = Scripts.validatorScript inst

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

type InputTxDatumsSchema =
  BlockchainActions
    .\/ Endpoint "start" ()
    .\/ Endpoint "close" ()

-- Simplify type for auction contract.
type InputTxDatumsContract a = Contract () InputTxDatumsSchema ContractError a

start :: InputTxDatumsContract ()
start = do
  logI "Starting"
  let scrInst = inst
      scr = validator
  utxoMap <- utxoAt scrAddress
  let lookups =
        Constraints.unspentOutputs utxoMap
          <> Constraints.scriptInstanceLookups scrInst
          <> Constraints.otherScript scr
      constraints =
        mustPayToTheScript StateA mempty
          <> mustPayToTheScript StateB mempty
  ledgerTx <- submitTxConstraintsWith lookups constraints
  void . awaitTxConfirmed . txId $ ledgerTx
  printUTxODatums scrAddress

close :: InputTxDatumsContract ()
close = do
  logI "Closing"
  let scrInst = inst
      scr = validator
  utxoMap <- utxoAt scrAddress
  let [oref1, oref2] = keys utxoMap
      lookups =
        Constraints.unspentOutputs utxoMap
          <> Constraints.scriptInstanceLookups scrInst
          <> Constraints.otherScript scr
      constraints =
        mustSpendScriptOutput oref1 unitRedeemer
          <> mustSpendScriptOutput oref2 unitRedeemer
          -- Note: In order to find datum of inputs, they must be added
          <> mustIncludeDatum (Datum $ PlutusTx.toData StateA)
          <> mustIncludeDatum (Datum $ PlutusTx.toData StateB)
          <> mustPayToTheScript Final mempty
  ledgerTx <- submitTxConstraintsWith lookups constraints
  logInfo ledgerTx
  void . awaitTxConfirmed . txId $ ledgerTx
  printUTxODatums scrAddress

endpoints :: InputTxDatumsContract ()
endpoints = (start' `select` close') >> endpoints
  where
    start' = endpoint @"start" >> start
    close' = endpoint @"close" >> close

-- | Tests
test :: IO ()
test = Emulator.runEmulatorTraceIO' def {showEvent=testShowEvent} def $ do
  h1 <- Emulator.activateContractWallet w1 endpoints
  -- Starting
  Extras.logInfo @String $ "Start"
  Emulator.callEndpoint @"start" h1 ()
  void $ Emulator.waitNSlots 2
  Extras.logInfo @String $ "Close"
  Emulator.callEndpoint @"close" h1 ()
  s <- Emulator.waitNSlots 1
  Extras.logInfo $ "Exit" ++ show s

w1 :: Wallet.Wallet
w1 = Wallet.Wallet 1

walletPubKeyHash :: Wallet.Wallet -> PubKeyHash
walletPubKeyHash = pubKeyHash . Wallet.walletPubKey

-- | General helpers
printUTxODatums :: Ledger.Address -> InputTxDatumsContract ()
printUTxODatums scrAddr = do
  utxoMap <- utxoAt $ scrAddr
  logI'' "UTxO count" "count" $ show (Map.size utxoMap)
  let datums :: [(Value, InputTxDatumsState)] =
        utxoMap
          ^.. folded
            . Control.Lens.to (\o -> (txOutValue $ txOutTxOut o,) <$> txOutTxDatum o)
            . _Just
            . Control.Lens.to (\(v, Datum d) -> (v,) <$> PlutusTx.fromData @InputTxDatumsState d)
            . _Just
  PlutusTx.Prelude.mapM_ (logI'' "UTxO datums" "datums") $ fmap show datums

logI :: Haskell.String -> Contract w s e ()
logI = logInfo @Haskell.String

-- TODO (sometime): Replace value tuples with HList.
logI' :: Haskell.String -> [(Haskell.String, Haskell.String)] -> Contract w s e ()
logI' t m = logInfo @Haskell.String $ t <> printKeyValues m
  where
    printKeyValues [] = ""
    printKeyValues m' = ": " <> mconcat (fmap kvToString m')
    kvToString (k, v) = k <> "=" <> show v

logI'' :: Haskell.String -> Haskell.String -> Haskell.String -> Contract w s e ()
logI'' t k v = logI' t [(k, v)]

testShowEvent :: EmulatorEvent' -> Maybe String
testShowEvent = \case
  UserThreadEvent (UserLog msg) -> Just $ "*** USER LOG: " <> msg
  InstanceEvent (ContractInstanceLog (ContractLog json) _ _) -> Just $ "*** CONTRACT LOG: " <> unpack (encodePretty json)
  InstanceEvent (ContractInstanceLog (StoppedWithError err) _ _) -> Just $ "*** CONTRACT STOPPED WITH ERROR: " <> show err
  InstanceEvent (ContractInstanceLog NoRequestsHandled _ _) -> Nothing
  InstanceEvent (ContractInstanceLog (HandledRequest _) _ _) -> Nothing
  InstanceEvent (ContractInstanceLog (CurrentRequests _) _ _) -> Nothing
  SchedulerEvent _ -> Nothing
  ChainIndexEvent _ _ -> Nothing
  WalletEvent _ _ -> Nothing
  ev -> Just . renderString . layoutPretty defaultLayoutOptions . pretty $ ev

