{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Note: Heavily relies on Auction in `plutus-use-cases`.
module ParallelAuction.ParallelAuctionTrace where

import Control.Lens ( (&), (.~), (<>~), Ixed(ix) )
import Data.Aeson ((.=))
import Data.Default ( Default(def) )
import Data.Functor (void)
import qualified Control.Monad.Freer.Extras as Extras
import Ledger ( PubKeyHash, Value, pubKeyHash )
import qualified Ledger.Value as Value
import ParallelAuction.ParallelAuction
    ( endpoints,
      ParallelAuctionParams(ParallelAuctionParams, pThreadCount,
                            pEndTime) )
import Plutus.Contract.Trace
    ( Wallet(Wallet), walletPubKey, defaultDist )
import Plutus.Trace.Emulator as Emulator
    ( activateContractWallet,
      callEndpoint,
      waitNSlots,
      waitUntilSlot,
      runEmulatorTraceIO',
      showEvent,
      initialChainState,
      EmulatorTrace,
      TraceConfig,
      EmulatorConfig )
import Utils.LoggingUtil

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
traceConfig = def {showEvent = showEventPretty}

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
  logI "Wallet 1 starts auction"
  callEndpoint @"start" h1 auction
  -- Bidding
  void $ waitUntilSlot 5
  logI "Wallet 2 bids"
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
  -- Start
  callEndpoint @"start" h1 auction
  void $ waitNSlots 1
  -- Closing
  void $ waitUntilSlot (pEndTime auction)
  callEndpoint @"close" h2 auction
  void $ waitNSlots 1
  Extras.logInfo @String "Exit"

testTraceSequentialBidding :: EmulatorTrace ()
testTraceSequentialBidding = do
  let auction = theAuction {pThreadCount = 1}
  h1 <- activateContractWallet w1 endpoints
  h2 <- activateContractWallet w2 endpoints
  h3 <- activateContractWallet w3 endpoints
  -- Starting
  logI "w1 starts"
  callEndpoint @"start" h1 auction
  -- Bidding
  void $ waitUntilSlot 5
  logI "Wallet 2 is bidding"
  callEndpoint @"bid" h2 (auction, 400)
  void $ waitNSlots 1
  logI "Wallet 3 is bidding"
  callEndpoint @"bid" h3 (auction, 500)
  -- Closing
  s <- waitUntilSlot (pEndTime auction)
  callEndpoint @"close" h3 auction
  void $ waitNSlots 1
  logI' "Exit" [ "slot" .= s]

testTraceParallelBidding :: EmulatorTrace ()
testTraceParallelBidding = do
  let auction = theAuction
  h1 <- activateContractWallet w1 endpoints
  h2 <- activateContractWallet w2 endpoints
  h3 <- activateContractWallet w3 endpoints
  h4 <- activateContractWallet w4 endpoints
  -- Starting
  logI "w1 starts"
  callEndpoint @"start" h1 auction
  -- Parallel Bidding
  void $ waitUntilSlot 5
  logI "Wallet 2 is bidding"
  callEndpoint @"bid" h2 (auction, 400)
  logI "Wallet 2 is bidding"
  callEndpoint @"bid" h4 (auction, 500)
  logI "Wallet 3 is bidding"
  callEndpoint @"bid" h3 (auction, 100)
  -- Closing
  void $ waitUntilSlot (pEndTime auction)
  callEndpoint @"close" h4 auction
  void $ waitNSlots 1
  logI "Exit"
