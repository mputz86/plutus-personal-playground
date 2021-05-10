{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils.LoggingUtil where

import Control.Lens (folded, to, (^..), _Just)
import Data.Aeson (ToJSON, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.HashMap.Strict as HashMap
import Data.List as List
  ( dropWhile,
    dropWhileEnd,
    filter,
    takeWhile,
  )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.String (String)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as LazyText
import Data.Text.Prettyprint.Doc (Pretty (..), annotate, defaultLayoutOptions, layoutPretty)
import Data.Text.Prettyprint.Doc.Render.String (renderString)
import Ledger
  ( Address,
    Datum (Datum),
    Tx (txInputs),
    TxIn (txInType),
    TxInType (ConsumeScriptAddress),
    TxOut (txOutValue),
    TxOutTx (txOutTxOut),
    txOutTxDatum,
  )
import Ledger.AddressMap (UtxoMap)
import Plutus.Contract as Contract
  ( AsContractError,
    Contract,
    HasBlockchainActions,
    logInfo,
    utxoAt,
  )
import Plutus.Trace.Emulator.Types
  ( ContractInstanceLog (ContractInstanceLog),
    ContractInstanceMsg
      ( ContractLog,
        CurrentRequests,
        HandledRequest,
        NoRequestsHandled,
        StoppedWithError
      ),
    UserThreadMsg (UserLog),
  )
import qualified PlutusTx
import PlutusTx.Prelude
  ( Bool (..),
    Functor (fmap),
    Maybe (..),
    fromMaybe,
    ($),
    (.),
    (<$>),
  )
import Prettyprinter (emptyDoc, line)
import Prettyprinter.Render.Terminal
  ( Color (Yellow),
    bold,
    color,
    renderLazy,
  )
import Text.Pretty.Simple
  ( OutputOptions (outputOptionsCompact),
    defaultOutputOptionsDarkBg,
    pShow,
    pShowOpt,
    pString,
  )
import Wallet.Emulator (EmulatorEvent')
import Wallet.Emulator.MultiAgent
  ( EmulatorEvent'
      ( ChainIndexEvent,
        InstanceEvent,
        SchedulerEvent,
        UserThreadEvent,
        WalletEvent
      ),
  )
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
    -- Try to parse error string as JSON.
    let errMsg = List.takeWhile (Haskell./= ' ') err
        errP = List.filter (Haskell./= '\\') . dropWhile (Haskell./= '{') . dropWhileEnd (Haskell./= '}') $ err
     in case Aeson.decode @Aeson.Value . pack $ errP of
          Just j -> Just . LazyText.unpack $ eventMetaData ("Contract " <> LazyText.pack errMsg) (Haskell.Right j)
          Nothing -> Just . LazyText.unpack $ eventMetaData "Contract" (Haskell.Left $ "ERROR " <> errP)
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
    eventMetaData t ev =
      case ev of
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
      _ -> emptyDoc
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

toLogT' :: Text.Text -> [(Text.Text, Aeson.Value)] -> Text.Text
toLogT' t m =
  LazyText.toStrict
    . LazyText.toLazyText
    . Aeson.encodeToTextBuilder
    $ toLogS t m

logI :: Text.Text -> Contract w s e ()
logI = Contract.logInfo

logI' :: Text.Text -> [(Text.Text, Aeson.Value)] -> Contract w s e ()
logI' t m = Contract.logInfo $ toLogS t m

logI'' :: ToJSON a => Text.Text -> Text.Text -> a -> Contract w s e ()
logI'' t k v = logI' t [k .= v]
