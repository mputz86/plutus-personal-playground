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


test :: IO ()
test = runEmulatorTraceIO' def emulatorConfig testTrace

testBidding :: IO ()
testBidding = runEmulatorTraceIO' def emulatorConfig $ do
  h1 <- activateContractWallet w1 endpoints
  h2 <- activateContractWallet w2 endpoints
  -- Starting
  Extras.logInfo @String $ "Wallet 1 starts auction"
  callEndpoint @"start" h1 theAuction
  -- Bidding
  void $ waitUntilSlot 5
  Extras.logInfo @String $ "Wallet 2 bids"
  callEndpoint @"bid" h2 (theAuction, 400)
  -- Closing
  void $ waitUntilSlot (pEndTime theAuction)
  -- FIXME Close with any wallet
  callEndpoint @"close" h1 theAuction
  s <- waitNSlots 1
  Extras.logInfo $ "Exit" ++ show s


w1, w2, w3 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3

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

testTrace :: EmulatorTrace ()
testTrace = do
  h1 <- activateContractWallet w1 endpoints
  h2 <- activateContractWallet w2 endpoints
  h3 <- activateContractWallet w3 endpoints
  -- Starting
  Extras.logInfo @String $ "w1 starts"
  callEndpoint @"start" h1 theAuction
  -- Bidding
  void $ waitUntilSlot 5
  Extras.logInfo @String $ "w2 is bidding"
  callEndpoint @"bid" h2 (theAuction, 400)
  callEndpoint @"bid" h3 (theAuction, 100)
  -- callEndpoint @"bid" h3 (theAuction, 300)
  -- Closing
  s <- waitUntilSlot (pEndTime theAuction)
  -- FIXME Close with any wallet
  callEndpoint @"close" h1 theAuction
  void $ waitNSlots 1
  Extras.logInfo $ "Exit" ++ show s
