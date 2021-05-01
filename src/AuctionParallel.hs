{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
module Plutus.Contracts.AuctionParallel(
    AuctionState(..),
    AuctionInput(..),
    BuyerSchema,
    SellerSchema,
    AuctionParams(..),
    HighestBid(..),
    auctionBuyer,
    auctionSeller,
    AuctionOutput(..),
    AuctionError(..)
    ) where

import           Control.Lens                     (makeClassyPrisms)
import           Data.Aeson                       (FromJSON, ToJSON)
import           Data.Monoid                      (Last (..))
import           Data.Semigroup.Generic           (GenericSemigroupMonoid (..))
import           GHC.Generics                     (Generic)
import           Ledger                           (Ada, PubKeyHash, Slot, Value)
import qualified Ledger
import qualified Ledger.Ada                       as Ada
import qualified Ledger.Constraints               as Constraints
import           Ledger.Constraints.TxConstraints (TxConstraints)
import qualified Ledger.Interval                  as Interval
import qualified Ledger.Typed.Scripts             as Scripts
import           Ledger.Typed.Tx                  (TypedScriptTxOut (..))
import           Ledger.Value                     (AssetClass)
import           Plutus.Contract
import           Plutus.Contract.StateMachine     (State (..), StateMachine (..), StateMachineClient,
                                                   StateMachineInstance (..), Void, WaitingResult (..))
import qualified Plutus.Contract.StateMachine     as SM
import           Plutus.Contract.Util             (loopM)
import qualified Plutus.Contracts.Currency        as Currency
import qualified PlutusTx                         as PlutusTx
import           PlutusTx.Prelude
import qualified Prelude                          as Haskell


-- Idea:
--
--
-- # Question
-- - How does Datum look like?
-- - How does spin offs look like?
-- - How does closing Tx look like
--
--
-- # Validator
-- For
--
-- # Endpoints
-- - Create
-- - Bid
-- - Close
--   - Anybody, incentivized by highest bidder
--
-- # Initiator / Closer
-- - Forge tokens
--
--
-- # Bidder
-- - Choose one of the possible UTxOs for bidding
-- - Maybe something with hash and wrap around number of availabe UTxO
-- - Look at all at once and choose a max
-- - Bid by extending one of the UTxOs
--
-- # Close
-- - Tx must have all UTxO of initial auction
-- - Auction collects highest bids of all threads and computes highest bit
-- - Pay back non-highest bid
--
-- # Test
-- - Allow multiple bidders in one Slot / at the same time
-- - Bids on same UTxO lead to some invalid Tx
-- - But most should go through if enough open UTxO
--
--
