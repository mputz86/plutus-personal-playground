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

import Control.Lens (folded, to, (^..), _Just, (^?), ix)
import Data.Aeson (ToJSON, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import Data.ByteString.Lazy.Char8 (pack)
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
import Data.Text.Prettyprint.Doc (Doc, Pretty (..), annotate, defaultLayoutOptions, layoutPretty)
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
  ( AnsiStyle,
    Color (Yellow),
    bold,
    color, renderLazy
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

-- | Function for showEvent, can be set when running an EmulatorTrace like this:
--     test :: EmulatorTrace () -> IO ()
--     test = runEmulatorTraceIO' def {showEvent = testShowEvent} def
--
--   Features:
--   - Prints out pretty, colorful JSONs to terminal
--   - Tries to extract JSON content from error and prints them pretty and colorful
--   - Any non-captured event is also colorful and pretty-printed
showEventPretty :: EmulatorEvent' -> Maybe Haskell.String
showEventPretty = \case
  UserThreadEvent (UserLog msg) ->
    Just $ toPrettyLog "User" (Aeson.String . Text.pack $ msg)
  InstanceEvent (ContractInstanceLog (ContractLog json) _ _) ->
    Just $ toPrettyLog "Contract" json
  InstanceEvent (ContractInstanceLog (StoppedWithError err) _ _) ->
    Just $ extractJson err
  InstanceEvent (ContractInstanceLog NoRequestsHandled _ _) -> Nothing
  InstanceEvent (ContractInstanceLog (HandledRequest _) _ _) -> Nothing
  InstanceEvent (ContractInstanceLog (CurrentRequests _) _ _) -> Nothing
  SchedulerEvent _ -> Nothing
  ChainIndexEvent _ _ -> Nothing
  WalletEvent _ _ -> Nothing
  ev -> Just . renderUnknown $ ev

toPrettyLog :: Text.Text -> Aeson.Value -> String
toPrettyLog t ev =
  case ev of
    (Aeson.String s) ->
      renderPretty $
        annotateEventType t <> annotateTitle s
    v -> renderPretty $ annotateEventType t <> jsonTitle v <> line <> pretty (jsonToString v)

annotateEventType :: Text.Text -> Doc AnsiStyle
annotateEventType t = annotate (color Yellow) . pretty $ "[" <> t <> "] "

annotateTitle :: Text.Text -> Doc AnsiStyle
annotateTitle = annotate (color Yellow <> bold) . pretty

jsonTitle :: Aeson.Value -> Doc AnsiStyle
jsonTitle (Aeson.Object m) = case m ^? ix "title" of
  Just (Aeson.String s) -> annotateTitle s
  _ -> emptyDoc
jsonTitle _ = emptyDoc

extractJson :: String -> String
extractJson msg =
  let prefix =
        List.dropWhileEnd (Haskell.== ' ')
          . List.takeWhile (Haskell./= '{')
          $ msg
      jsonS =
        List.filter (Haskell./= '\\')
          . dropWhile (Haskell./= '{')
          . dropWhileEnd (Haskell./= '}')
          $ msg
   in case Aeson.decode @Aeson.Value . pack $ jsonS of
        Just j -> toPrettyLog ("Contract " <> Text.pack prefix) j
        Nothing -> toPrettyLog "Contract" (Aeson.String . Text.pack $ "ERROR " <> msg)

renderUnknown :: EmulatorEvent' -> String
renderUnknown =
  renderPretty
    . pretty
    . pShowOpt (defaultOutputOptionsDarkBg {outputOptionsCompact = True})

jsonToString :: Aeson.Value -> LazyText.Text
jsonToString = pString . LazyText.unpack . LazyText.toLazyText . Aeson.encodeToTextBuilder

renderPretty :: Doc AnsiStyle -> String
renderPretty = LazyText.unpack . renderLazy . layoutPretty defaultLayoutOptions

--
--
-- Logging helpers for Contract
--
--

logI :: Text.Text -> Contract w s e ()
logI = Contract.logInfo

logI' :: Text.Text -> [(Text.Text, Aeson.Value)] -> Contract w s e ()
logI' t m = Contract.logInfo $ toLogS t m

logI'' :: ToJSON a => Text.Text -> Text.Text -> a -> Contract w s e ()
logI'' t k v = logI' t [k .= v]

-- | Logs all inputs of the given transaction. Together with datum.
logInputs :: forall state w s e. (PlutusTx.IsData state, ToJSON state) => Tx -> Contract w s e ()
logInputs ledgerTx = do
  let txIns =
        fmap (PlutusTx.fromData @state)
          . extractDatum
          . txInType
          <$> Set.toList (txInputs ledgerTx)
  logI'' "Transaction inputs" "inputs" txIns
  where
    extractDatum (ConsumeScriptAddress _ _ (Datum d)) = Just d
    extractDatum _ = Nothing

-- | Logs value and datum of UTxOs
logUtxos ::
  forall state w s e.
  (AsContractError e, HasBlockchainActions s, PlutusTx.IsData state, ToJSON state) =>
  Ledger.Address ->
  Contract w s e ()
logUtxos scrAddr = do
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
      "utxo datums" .= datums
    ]

-- | Logs the UTxO count
logUtxoSize :: Maybe Text.Text -> UtxoMap -> Contract w s e ()
logUtxoSize title utxoMap =
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
