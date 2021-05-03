{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Note: Heavily relies on Auction in `plutus-use-cases`.
module ParallelAuctionTrace where

import Control.Lens
import Control.Monad.Freer.Extras as Extras
import Data.Functor (void)
import Ledger
import ParallelAuction
import Plutus.Trace.Emulator as Emulator
import Plutus.Contract.Trace
import Wallet.Emulator.Wallet as Wallet
import qualified Ledger.Value                       as Value
import Data.Default


test :: EmulatorTrace () -> IO ()
test = runEmulatorTraceIO' def emulatorConfig

testX :: IO ()
testX = runEmulatorTraceIO' def emulatorConfig testTraceParallelBid

testSimpleBidding :: IO ()
testSimpleBidding = runEmulatorTraceIO' def emulatorConfig $ do
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


w1, w2, w3, w4 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3
w4 = Wallet 4

walletPubKeyHash :: Wallet -> PubKeyHash
walletPubKeyHash = pubKeyHash . walletPubKey

threadCount :: Integer
threadCount = 3

theAuction :: ParallelAuctionParams
theAuction = ParallelAuctionParams (walletPubKeyHash w1) offeredToken 20 threadCount

-- | The token that we are auctioning off.
offeredToken :: Value
offeredToken =
  -- "ffff" is not a valid MPS hash. But this doesn't matter because we
  -- never try to forge any value of "ffff" using a script.
  -- This currency is created by the initial transaction.
  Value.singleton "ffff" "token" 1

-- | 'CheckOptions' that inclues 'theToken' in the initial distribution of wallet 1.
emulatorConfig :: EmulatorConfig
emulatorConfig =
  let d = defaultDist & ix w1 <>~ offeredToken
   in def & Emulator.initialChainState .~ Left d

-- Tests
-- - What if contract started twice with same config, i.e. same validator

testTraceParallelBid :: EmulatorTrace ()
testTraceParallelBid = do
  let auction = theAuction
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
  Extras.logInfo @String $ "Wallet 3 is bidding"
  callEndpoint @"bid" h3 (auction, 100)
  -- Closing
  s <- waitUntilSlot (pEndTime auction)
  -- FIXME Close with any wallet
  callEndpoint @"close" h1 auction
  void $ waitNSlots 1
  Extras.logInfo $ "Exit" ++ show s

testTraceSequentialBid :: EmulatorTrace ()
testTraceSequentialBid = do
  let auction = theAuction{pThreadCount = 1}
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
  -- FIXME Close with any wallet
  --callEndpoint @"close" h1 auction
  void $ waitNSlots 1
  Extras.logInfo $ "Exit" ++ show s
