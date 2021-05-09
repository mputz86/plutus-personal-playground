-- TODO Remove unused extensions
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LoggingUtil where

-- TODO Remove unused imports
import Control.Lens hiding ((.=))
import Control.Monad hiding (fmap)
import Control.Monad.Freer.Extras as Extras
import Data.Aeson ((.=))
import Data.Aeson as Aeson (FromJSON, ToJSON, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Text as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Default
import Data.Functor (void)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (hash)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import Data.Text.Prettyprint.Doc (Pretty (..), annotate, defaultLayoutOptions, layoutPretty)
import Data.Text.Prettyprint.Doc.Render.String (renderString)
import GHC.Generics (Generic)
import Ledger
import Ledger.Ada as Ada
import Ledger.AddressMap (UtxoMap)
import Ledger.Constraints as Constraints
  ( TxConstraints,
    checkScriptContext,
    mustIncludeDatum,
    mustPayToPubKey,
    mustPayToTheScript,
    mustSpendScriptOutput,
    mustValidateIn,
    otherScript,
    scriptInstanceLookups,
    unspentOutputs,
  )
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Value
import Plutus.Contract as Contract
  ( AsContractError (_ContractError),
    BlockchainActions,
    Contract,
    Endpoint,
    HasBlockchainActions,
    awaitTxConfirmed,
    endpoint,
    logInfo,
    ownPubKey,
    select,
    submitTxConstraints,
    submitTxConstraintsWith,
    throwError,
    utxoAt,
  )
import Plutus.Contract.Trace
import Plutus.Contract.Types (ContractError)
import qualified Plutus.Contracts.Currency as Currency
import Plutus.Trace.Emulator as Emulator
import Plutus.Trace.Emulator.Types
import qualified Plutus.V1.Ledger.Interval as Interval
import qualified Plutus.V1.Ledger.Value as Value
import qualified PlutusTx
import PlutusTx.AssocMap as AssocMap
import qualified PlutusTx.Builtins
import PlutusTx.Prelude
  ( AdditiveGroup ((-)),
    AdditiveSemigroup ((+)),
    Applicative (pure),
    Bool (..),
    Eq ((==)),
    Functor (fmap),
    Int,
    Integer,
    Integral (mod),
    Maybe (..),
    Monoid (mempty),
    Ord (compare, (<), (<=)),
    Ordering (EQ, GT),
    Show (show),
    const,
    fromIntegral,
    fromMaybe,
    id,
    isJust,
    mapM_,
    maximum,
    mconcat,
    not,
    replicate,
    snd,
    trace,
    traceIfFalse,
    ($),
    (&&),
    (.),
    (<$>),
  )
import Prettyprinter (emptyDoc, line)
import Prettyprinter.Render.Terminal
import Text.Pretty.Simple
import Wallet.Emulator
import Wallet.Emulator.MultiAgent
import Prelude (Semigroup (..))
import qualified Prelude as Haskell

-- Emulator Logging

-- TODO Make nicer
-- TODO Code style per log-type and info
testShowEvent :: EmulatorEvent' -> Maybe Haskell.String
testShowEvent = \case
  UserThreadEvent (UserLog msg) ->
    Just . LazyText.unpack $ eventMetaData "User" (Haskell.Left msg)
  InstanceEvent (ContractInstanceLog (ContractLog json) _ _) ->
    Just . LazyText.unpack $ eventMetaData "Contract" (Haskell.Right json)
  InstanceEvent (ContractInstanceLog (StoppedWithError err) _ _) ->
    Just . LazyText.unpack $ eventMetaData "Contract" (Haskell.Left $ "ERROR " <> show err)
  InstanceEvent (ContractInstanceLog NoRequestsHandled _ _) -> Nothing
  InstanceEvent (ContractInstanceLog (HandledRequest _) _ _) -> Nothing
  InstanceEvent (ContractInstanceLog (CurrentRequests _) _ _) -> Nothing
  SchedulerEvent _ -> Nothing
  ChainIndexEvent _ _ -> Nothing
  WalletEvent _ _ -> Nothing
  ev ->
    Just
      . renderString
      . layoutPretty defaultLayoutOptions
      . pretty
      $ pShowOpt (defaultOutputOptionsDarkBg {outputOptionsCompact = True}) ev
  where
    eventMetaData :: LazyText.Text -> Haskell.Either String Aeson.Value -> LazyText.Text
    eventMetaData t v =
      case v of
        Haskell.Left s ->
          renderLazy . layoutPretty defaultLayoutOptions $
            annotateEventType t <> annotateTitle (LazyText.pack s)
        Haskell.Right v -> renderLazy . layoutPretty defaultLayoutOptions $ annotateEventType t <> jsonTitle v <> line <> pretty (jsonToString v)
    jsonToString =
      pString
        . LazyText.unpack
        . LazyText.toLazyText
        . Aeson.encodeToTextBuilder
    annotateEventType t = annotate (color Yellow) . pretty $ "[" <> t <> "] "
    annotateTitle t = annotate (color Yellow <> bold) . pretty $ t
    jsonTitle (Aeson.Object m) = case HashMap.lookup "title" m of
      Just (Aeson.String s) -> annotateTitle s
      Nothing -> emptyDoc
    jsonTitle _ = emptyDoc
--
-- Contract Logging

logInputs :: forall state w s e. (PlutusTx.IsData state, ToJSON state) => Tx -> Contract w s e ()
logInputs ledgerTx = do
  let txIns =
        fmap (PlutusTx.fromData @state)
          . extractDatum
          . txInType
          <$> Set.toList (txInputs ledgerTx)
  logI'' "Tx inputs" "tx inputs" txIns
  where
    extractDatum (ConsumeScriptAddress _ _ (Datum d)) = Just d
    extractDatum _ = Nothing

printUtxos ::
  forall state w s e.
  (AsContractError e, HasBlockchainActions s, PlutusTx.IsData state, ToJSON state) =>
  Ledger.Address ->
  Contract w s e ()
printUtxos scrAddr = do
  utxoMap <- utxoAt scrAddr
  let datums =
        utxoMap
          ^.. folded
            . Control.Lens.to (\o -> (txOutValue $ txOutTxOut o,) <$> txOutTxDatum o)
            . _Just
            . Control.Lens.to (\(v, Datum d) -> (v,) <$> PlutusTx.fromData @state d)
            . _Just
  logI'
    "UTxOs"
    [ "script address" .= scrAddr,
      "UTxO count" .= Map.size utxoMap,
      "datum" .= datums
    ]

logUTxOSize :: Maybe Text.Text -> UtxoMap -> Contract w s e ()
logUTxOSize title utxoMap =
  let t = fromMaybe "UTxO size of script" title
   in logI'' t "size" $ Map.size utxoMap

toLogS :: Text.Text -> [(Text.Text, Aeson.Value)] -> Aeson.Value
toLogS t m = Aeson.object $ "title" .= t : m

toLogT :: Text.Text -> [(Text.Text, Aeson.Value)] -> Text.Text
toLogT t m = LazyText.toStrict . pShow $ toLogS t m

logI :: Text.Text -> Contract w s e ()
logI = Contract.logInfo

logI' :: Text.Text -> [(Text.Text, Aeson.Value)] -> Contract w s e ()
logI' t m = Contract.logInfo $ toLogS t m

logI'' :: ToJSON a => Text.Text -> Text.Text -> a -> Contract w s e ()
logI'' t k v = logI' t [k .= v]
