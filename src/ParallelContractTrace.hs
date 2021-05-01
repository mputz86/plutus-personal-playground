{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module ParallelContractTrace where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Ledger
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet as Wallet

import ParallelContract

test :: IO ()
test = runEmulatorTraceIO testTrace

w1, w2, w3 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3

walletPubKeyHash :: Wallet -> PubKeyHash
walletPubKeyHash = pubKeyHash . walletPubKey

testTrace :: EmulatorTrace ()
testTrace = do
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    -- h3 <- activateContractWallet w3 endpoints
    Extras.logInfo $ "w1 starts"
    callEndpoint @"start" h1 ()
    void $ waitUntilSlot 5
    Extras.logInfo $ "w2 is bidding"
    callEndpoint @"bid" h2 10
    -- callEndpoint @"bid" h3 10
    void $ waitUntilSlot 5
    s <- waitNSlots 1
    Extras.logInfo $ "END" ++ show s
