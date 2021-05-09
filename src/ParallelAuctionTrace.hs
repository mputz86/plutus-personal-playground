{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Note: Heavily relies on Auction in `plutus-use-cases`.
module ParallelAuctionTrace where

import Text.Pretty.Simple (pPrint, pString, pShow)
import Text.Pretty.Simple
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.Text as Aeson
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Text.Prettyprint.Doc (Pretty (..), annotate, defaultLayoutOptions, layoutPretty)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Text.Lazy.Builder as LazyText
import Data.Text.Prettyprint.Doc.Render.String (renderString)
import Prettyprinter.Render.Terminal
import Wallet.Emulator
import Wallet.Emulator.MultiAgent
import Control.Lens hiding ((.=))
import Control.Monad.Freer.Extras as Extras
import Data.Default
import Data.Functor (void)
import Ledger
import qualified Ledger.Value as Value
import ParallelAuction
import Plutus.Contract.Trace
import Plutus.Trace.Emulator as Emulator
import Plutus.Trace.Emulator.Types
import LoggingUtil

test :: EmulatorTrace () -> IO ()
test = runEmulatorTraceIO' traceConfig emulatorConfig

testSimpleBidding :: IO ()
testSimpleBidding = test testTraceSimpleBidding

testStartCloseBidding :: IO ()
testStartCloseBidding = test testTraceStartCloseBidding

testSequentialBidding :: IO ()
testSequentialBidding = test testTraceSequentialBidding

testParallelBidding :: IO ()
testParallelBidding = test testTraceParallelBidding

w1, w2, w3, w4 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3
w4 = Wallet 4
w5 = Wallet 5

walletPubKeyHash :: Wallet -> PubKeyHash
walletPubKeyHash = pubKeyHash . walletPubKey

defaultThreadCount :: Integer
defaultThreadCount = 3

theAuction :: ParallelAuctionParams
theAuction = ParallelAuctionParams (walletPubKeyHash w1) offeredToken 20 defaultThreadCount

-- | The token that we are auctioning off (taken from use-case Auction)
offeredToken :: Value
offeredToken = Value.singleton "ffff" "token" 1

-- | Pay the offered token to wallet 1 at initialisation.
traceConfig :: TraceConfig
traceConfig = def { showEvent = testShowEvent }

emulatorConfig :: EmulatorConfig
emulatorConfig =
  let d = defaultDist & ix w1 <>~ offeredToken
   in def & Emulator.initialChainState .~ Left d

testTraceSimpleBidding :: EmulatorTrace ()
testTraceSimpleBidding = do
  let auction = theAuction
  h1 <- activateContractWallet w1 endpoints
  h2 <- activateContractWallet w2 endpoints
  -- Starting
  Extras.logInfo @String $ "Wallet 1 starts auction"
  callEndpoint @"start" h1 auction
  -- Bidding
  void $ waitUntilSlot 5
  Extras.logInfo @String $ "Wallet 2 bids"
  callEndpoint @"bid" h2 (auction, 400)
  -- Closing
  void $ waitUntilSlot (pEndTime auction)
  -- FIXME Close with any wallet
  callEndpoint @"close" h1 auction
  s <- waitNSlots 1
  Extras.logInfo $ "Exit" ++ show s

testTraceStartCloseBidding :: EmulatorTrace ()
testTraceStartCloseBidding = do
  let auction = theAuction
  h1 <- activateContractWallet w1 endpoints
  h2 <- activateContractWallet w2 endpoints
  -- Closing
  void $ waitUntilSlot (pEndTime auction)
  callEndpoint @"close" h2 auction
  s <- void $ waitNSlots 1
  Extras.logInfo $ "Exit" ++ show s

testTraceSequentialBidding :: EmulatorTrace ()
testTraceSequentialBidding = do
  let auction = theAuction {pThreadCount = 1}
  h1 <- activateContractWallet w1 endpoints
  h2 <- activateContractWallet w2 endpoints
  h3 <- activateContractWallet w3 endpoints
  -- Starting
  Extras.logInfo @String $ "w1 starts"
  callEndpoint @"start" h1 auction
  -- Bidding
  void $ waitUntilSlot 5
  Extras.logInfo @String $ "Wallet 2 is bidding"
  callEndpoint @"bid" h2 (auction, 400)
  void $ waitNSlots 1
  Extras.logInfo @String $ "Wallet 3 is bidding"
  callEndpoint @"bid" h3 (auction, 500)
  -- Closing
  s <- waitUntilSlot (pEndTime auction)
  callEndpoint @"close" h3 auction
  void $ waitNSlots 1
  Extras.logInfo $ "Exit" ++ show s

testTraceParallelBidding :: EmulatorTrace ()
testTraceParallelBidding = do
  let auction = theAuction
  h1 <- activateContractWallet w1 endpoints
  h2 <- activateContractWallet w2 endpoints
  h3 <- activateContractWallet w3 endpoints
  h4 <- activateContractWallet w4 endpoints
  -- Starting
  Extras.logInfo @String $ "w1 starts"
  callEndpoint @"start" h1 auction
  -- Parallel Bidding
  void $ waitUntilSlot 5
  Extras.logInfo @String $ "Wallet 2 is bidding"
  callEndpoint @"bid" h2 (auction, 400)
  Extras.logInfo @String $ "Wallet 2 is bidding"
  callEndpoint @"bid" h4 (auction, 500)
  Extras.logInfo @String $ "Wallet 3 is bidding"
  callEndpoint @"bid" h3 (auction, 100)
  -- Closing
  void $ waitUntilSlot (pEndTime auction)
  callEndpoint @"close" h4 auction
  void $ waitNSlots 1
  Extras.logInfo @String "Exit"

