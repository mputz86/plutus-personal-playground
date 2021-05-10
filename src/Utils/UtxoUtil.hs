{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Useful functions for handling UTxOs. Both in off-chain and on-chain part.
module Utils.UtxoUtil where

import qualified Data.Map as Map
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Contexts
import Plutus.V1.Ledger.Tx
import qualified PlutusTx

-- | Abstracts UTxO access for fold and lookup away from off-chain and on-chain data structure.
data UtxoAccessor a = UtxoAccessor
  { aFoldF :: forall b. (b -> (TxOutRef, TxOut) -> b) -> b -> b,
    aLookupDataF :: (TxOutRef, DatumHash) -> Maybe Datum
  }

-- | Lookup a datum. Provide two identifiers so the accessor for off-chain and on-chain can
--   choose the appropriate one.
{-# INLINEABLE lookupData #-}
lookupData :: forall d a. PlutusTx.IsData d => UtxoAccessor a -> (TxOutRef, DatumHash) -> Maybe d
lookupData UtxoAccessor {..} rd = do
  Datum d <- aLookupDataF rd
  PlutusTx.fromData @d d

-- | Accessor for 'UtxoMap', so off-chain.
newUtxoMapAccessor :: Map.Map TxOutRef TxOutTx -> UtxoAccessor a
newUtxoMapAccessor utxoMap =
  UtxoAccessor
    { aFoldF = foldF,
      aLookupDataF = lookupDataF
    }
  where
    foldF :: (b -> (TxOutRef, TxOut) -> b) -> b -> b
    foldF f i = Map.foldlWithKey' (\a k u -> f a (k, txOutTxOut u)) i utxoMap
    lookupDataF (oref, _) = do
      txOutTx <- Map.lookup oref utxoMap
      txOutTxDatum txOutTx

-- | Accessor for 'TxInfo' from 'ScriptContext', so on-chain.
{-# INLINEABLE newUtxoTxInfoAccessor #-}
newUtxoTxInfoAccessor :: TxInfo -> UtxoAccessor a
newUtxoTxInfoAccessor txInfo =
  UtxoAccessor
    { aFoldF = foldF,
      aLookupDataF = lookupDataF
    }
  where
    foldF :: (b -> (TxOutRef, TxOut) -> b) -> b -> b
    foldF f i = foldl (\a (TxInInfo r o) -> f a (r, o)) i $ txInfoInputs txInfo
    lookupDataF (_, dh) = findDatum dh txInfo
