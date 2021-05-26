{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

--
-- Logging utility for improving logging in 'Contract' and 'EmulatorTrace'.
--
-- Usage examples can be found in "ParallelAuction.ParallelAuction" and "ParallelAuction.ParallelAuctionTrace".
--
-- Required dependencies (in addition to standard)
--                   , bytestring
--                   , data-default
--                   , freer-simple
--                   , lens
--                   , lens-aeson
--                   , pretty-simple
--                   , prettyprinter
--                   , prettyprinter-ansi-terminal
--

module Utils.LoggingUtil where

import Control.Lens (At (at), folded, ix, to, (%~), (&), (.~), (?~), (^..), (^?))
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Extras (LogMsg)
import qualified Control.Monad.Freer.Extras as Extras
import Data.Aeson (ToJSON, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens (key, _Object, _String)
import qualified Data.Aeson.Text as Aeson
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Char (isAlpha)
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
    Datum (Datum, getDatum),
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
    logDebug,
    logError,
    logInfo,
    logWarn,
    utxoAt,
  )
import Plutus.Trace.Emulator.Types
  ( ContractInstanceLog (ContractInstanceLog),
    ContractInstanceMsg
      ( ContractLog,
        CurrentRequests,
        HandledRequest,
        NoRequestsHandled,
        ReceiveEndpointCall,
        StoppedWithError
      ),
    UserThreadMsg (UserLog),
  )
import qualified PlutusTx
import PlutusTx.Prelude
  ( Bool (..),
    Functor (fmap),
    Maybe (..),
    Monad,
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
import Prelude (Semigroup (..), maybe, not, (/=))

--

-- * Enhancing 'EmulatorTrace'
--
-- Provides with 'showEventPretty' a replacement for the default 'showEvent' in 'TraceConfig'.
--
-- __Features:__
-- - Prints out pretty, colorful JSONs to terminal (and repl)
-- - Tries to extract JSON content from error and prints them pretty and colorful (by default they are Strings)
-- - Any non-captured event is also colorful and pretty-printed
--
-- Can be set when running an EmulatorTrace like this:
-- > test :: EmulatorTrace () -> IO ()
-- > test = runEmulatorTraceIO' def {showEvent = testShowEvent} def
--
--
-- * Unification of logging in 'Contract' and 'EmulatorTrace' monad
--
-- Unifies logging in 'Contract' and 'EmulatorTrace', provides same methods for both monads: 'logI', 'logD', ..., 'logI'' .
--
--
-- * Support for Logging of key-values
--
-- Supports the (nice, structured) logging with key values.
-- Note: Uses the (.=) operator of 'Data.Aeson' to construct key-value pairs.
--
-- Example:
--
-- > logI'
-- >   "UTxOs"
-- >   [ "script address" .= scrAddr,
-- >     "UTxO count" .= Map.size utxoMap,
-- >     "UTxO datums" .= datums
-- >   ]
--
--
-- * Helper for logging UTxOs in 'Contract'
--
-- Adds some special functions for logging UTxOs, inputs etc. of a transactions: 'logInputs', 'logUtxos', 'logUtxoCount'.
--
--
-- * Missing features / TODO s
--
-- - Color based on severity of logging
-- - Minimize some standard loggings, extract valuable information (like 'TxnValidate')
--

-- | Function to show events. Can be set in 'TraceConfig' to 'showEvent'.
showEventPretty :: EmulatorEvent' -> Maybe String
showEventPretty = \case
  UserThreadEvent (UserLog msg) -> Just . showUserLog $ msg
  InstanceEvent (ContractInstanceLog (ContractLog json) _ _) -> Just . showContractLog $ json
  InstanceEvent (ContractInstanceLog (StoppedWithError err) _ _) -> Just . showContractErrorLog $ err
  InstanceEvent (ContractInstanceLog NoRequestsHandled _ _) -> Nothing
  InstanceEvent (ContractInstanceLog (HandledRequest _) _ _) -> Nothing
  InstanceEvent (ContractInstanceLog (CurrentRequests _) _ _) -> Nothing
  InstanceEvent (ContractInstanceLog (ReceiveEndpointCall json) _ _) -> Just . showContractRequestLog $ json
  SchedulerEvent _ -> Nothing
  ChainIndexEvent _ _ -> Nothing
  WalletEvent _ _ -> Nothing
  ev -> Just . showUnknownLog $ ev
  where
    showUserLog msg =
      toPrettyLog "User" $ case parseJson msg of
        Nothing -> stringToJson msg
        Just j -> j
    showContractLog json =
      toPrettyLog "Contract" json
    showContractErrorLog err =
      toPrettyLog "Contract" $ case parseJsonFromError err of
        Nothing -> stringToJson err
        Just (p, j) -> prependPrefixToTitle p j
    showContractRequestLog =
       toPrettyLog "Contract" . prettifyContractReceiveEndpointCall
    showUnknownLog ev = renderUnknown ev

    prependPrefixToTitle "" json = json
    prependPrefixToTitle p json = json & _Object . at "title" %~ \case
       Just (Aeson.String t) -> Just . Aeson.String $ Text.pack p <> ": " <> t
       Nothing -> Just . stringToJson $ p
       v -> v

toPrettyLog :: Text.Text -> Aeson.Value -> String
toPrettyLog t ev =
  case ev of
    (Aeson.String s) ->
      renderPretty $
        annotateEventType t
          <> annotateTitle s
    v ->
      renderPretty $
        annotateEventType t
          <> maybe emptyDoc annotateTitle (extractJsonTitle v)
          <> line
          <> pretty (jsonToPrettyString . removeJsonTitle $ v)

annotateEventType :: Text.Text -> Doc AnsiStyle
annotateEventType t = annotate (color Yellow) . pretty $ "[" <> t <> "] "

annotateTitle :: Text.Text -> Doc AnsiStyle
annotateTitle = annotate (color Yellow <> bold) . pretty

prettifyContractReceiveEndpointCall :: Aeson.Value -> Aeson.Value
prettifyContractReceiveEndpointCall json =
  let title =
        Aeson.String $
          "Processing endpoint '"
            <> fromMaybe "???" (json ^? key "tag" . _String)
            <> "'"
      unwrappedValue = json ^? key "value" . key "unEndpointValue"
   in json & _Object . at "title" ?~ title
        & _Object . at "tag" .~ Nothing
        & _Object . at "value" .~ unwrappedValue

-- | Extracts a title from the JSON, otherwise returns an empty doc.
extractJsonTitle :: Aeson.Value -> Maybe Text.Text
extractJsonTitle (Aeson.Object m) = case m ^? ix "title" of
  Just (Aeson.String s) -> Just s
  _ -> Nothing
extractJsonTitle _ = Nothing

removeJsonTitle :: Aeson.Value -> Aeson.Value
removeJsonTitle v = v & _Object . at "title" .~ Nothing

parseJson :: String -> Maybe Aeson.Value
parseJson = Aeson.decode @Aeson.Value . pack

-- | Try to extract json from an error string.
--   Should be the error name with an escaped JSON object.
--   Note: Aggressively replaces all '\' with nothing, so this may replace too much.
--   FIXME Depending how far this will go, switch to Parser lib before making it too complicated.
parseJsonFromError :: String -> Maybe (String, Aeson.Value)
parseJsonFromError msg =
  let prefix =
          List.dropWhileEnd (not . isAlpha)
          .  List.takeWhile (/= '{') $ msg
      jsonS =
        List.filter (/= '\\')
          . dropWhile (/= '{')
          . dropWhileEnd (/= '}')
          $ msg
   in case Aeson.decode @Aeson.Value . pack $ jsonS of
        Just j -> Just (prefix, j)
        Nothing -> Nothing

--
--
-- Logging helpers for Contract and Emulator
--
--

data LogLevel = D | I | W | E

class (Monad m) => HasLogging m where
  log :: ToJSON a => LogLevel -> a -> m ()

  -- Note: Cannot be ToJSON since want to allow to provide any type of value
  log' :: LogLevel -> Text.Text -> [(Text.Text, Aeson.Value)] -> m ()
  log' l t m = log l $ toLogS t m

  logD :: Text.Text -> m ()
  logD t = log I (Aeson.String t)

  logD' :: Text.Text -> [(Text.Text, Aeson.Value)] -> m ()
  logD' = log' D

  logI :: Text.Text -> m ()
  logI t = log I (Aeson.String t)

  logI' :: Text.Text -> [(Text.Text, Aeson.Value)] -> m ()
  logI' = log' I

  logW :: Text.Text -> m ()
  logW t = log W (Aeson.String t)

  logW' :: Text.Text -> [(Text.Text, Aeson.Value)] -> m ()
  logW' = log' W

  logE :: Text.Text -> m ()
  logE t = log E (Aeson.String t)

  logE' :: Text.Text -> [(Text.Text, Aeson.Value)] -> m ()
  logE' = log' E

instance HasLogging (Contract w s e) where
  log D = Contract.logDebug
  log I = Contract.logInfo
  log W = Contract.logWarn
  log E = Contract.logError

instance Member (LogMsg String) effs => HasLogging (Eff effs) where
  log D = Extras.logDebug . jsonToString
  log I = Extras.logInfo . jsonToString
  log W = Extras.logWarn . jsonToString
  log E = Extras.logError . jsonToString

-- | Logs all inputs of the given transaction. Together with datum.
logInputs :: forall state w s e. (PlutusTx.IsData state, ToJSON state) => Tx -> Contract w s e ()
logInputs ledgerTx = do
  let txIns =
        fmap (PlutusTx.fromData @state)
          . extractDatum
          . txInType
          <$> Set.toList (txInputs ledgerTx)
  logI' "Transaction inputs" ["inputs" .= txIns]
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
            . Control.Lens.to (\o -> (txOutValue $ txOutTxOut o, txOutTxDatum o))
            -- Map second part of tuple to typed datum
            . Control.Lens.to (fmap $ fmap (PlutusTx.fromData @state . getDatum))
  logI'
    "UTxOs"
    [ "script address" .= scrAddr,
      "UTxO count" .= Map.size utxoMap,
      "utxo datums" .= datums
    ]

-- | Logs the UTxO count
logUtxoCount :: Maybe Text.Text -> UtxoMap -> Contract w s e ()
logUtxoCount title utxoMap =
  let t = fromMaybe "UTxO size of script" title
   in logI' t ["size" .= Map.size utxoMap]

--
--
-- Util functions
--
--

toLogS :: ToJSON a => Text.Text -> [(Text.Text, a)] -> Aeson.Value
toLogS t vs = Aeson.object $ "title" .= t : fmap (fmap Aeson.toJSON) vs

toLogT :: Text.Text -> [(Text.Text, Aeson.Value)] -> Text.Text
toLogT t m = LazyText.toStrict . pShow $ toLogS t m

toLogT' :: Text.Text -> [(Text.Text, Aeson.Value)] -> Text.Text
toLogT' t m =
  LazyText.toStrict
    . LazyText.toLazyText
    . Aeson.encodeToTextBuilder
    $ toLogS t m

renderUnknown :: EmulatorEvent' -> String
renderUnknown =
  renderPretty
    . pretty
    . pShowOpt (defaultOutputOptionsDarkBg {outputOptionsCompact = True})

renderPretty :: Doc AnsiStyle -> String
renderPretty = LazyText.unpack . renderLazy . layoutPretty defaultLayoutOptions

jsonToPrettyString :: ToJSON a => a -> String
jsonToPrettyString = LazyText.unpack . pString . BSL.unpack . Aeson.encode . Aeson.toJSON

jsonToString :: ToJSON a => a -> String
jsonToString = BSL.unpack . Aeson.encode . Aeson.toJSON

jsonToText :: ToJSON a => a -> LazyText.Text
jsonToText = pString . LazyText.unpack . LazyText.toLazyText . Aeson.encodeToTextBuilder . Aeson.toJSON

stringToJson :: String -> Aeson.Value
stringToJson = Aeson.String . Text.pack
