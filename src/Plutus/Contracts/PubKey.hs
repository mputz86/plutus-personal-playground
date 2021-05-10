{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

-- | A "pay-to-pubkey" transaction output implemented as a Plutus
--   contract. This is useful if you need something that behaves like
--   a pay-to-pubkey output, but is not (easily) identified by wallets
--   as one.
module Plutus.Contracts.PubKey (pubKeyContract, scriptInstance, PubKeyError (..), AsPubKeyError (..)) where

import Control.Lens (makeClassyPrisms, review)
import Control.Monad.Error.Lens (throwing)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Ledger
  ( PubKeyHash,
    ScriptContext (scriptContextTxInfo),
    TxOut (txOutAddress),
    TxOutRef,
    TxOutTx (..),
    Value,
    txId,
    unspentOutputsTx,
  )
import qualified Ledger.Constraints as Constraints
import Ledger.Contexts as V (txSignedBy)
import Ledger.Typed.Scripts (ScriptInstance)
import qualified Ledger.Typed.Scripts as Scripts
import Plutus.Contract as Contract
  ( AsContractError (_ContractError),
    Contract,
    ContractError,
    HasTxConfirmation,
    HasWriteTx,
    awaitTxConfirmed,
    mapError,
    submitTxConstraints,
  )
import qualified PlutusTx

mkValidator :: PubKeyHash -> () -> () -> ScriptContext -> Bool
mkValidator pk' _ _ p = V.txSignedBy (scriptContextTxInfo p) pk'

data PubKeyContract

instance Scripts.ScriptType PubKeyContract where
  type RedeemerType PubKeyContract = ()
  type DatumType PubKeyContract = ()

scriptInstance :: PubKeyHash -> Scripts.ScriptInstance PubKeyContract
scriptInstance =
  Scripts.validatorParam @PubKeyContract
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator

data PubKeyError
  = ScriptOutputMissing PubKeyHash
  | MultipleScriptOutputs PubKeyHash
  | PKContractError ContractError
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''PubKeyError

instance AsContractError PubKeyError where
  _ContractError = _PKContractError

-- | Lock some funds in a 'PayToPubKey' contract, returning the output's address
--   and a 'TxIn' transaction input that can spend it.
pubKeyContract ::
  forall w s e.
  ( HasWriteTx s,
    HasTxConfirmation s,
    AsPubKeyError e
  ) =>
  PubKeyHash ->
  Value ->
  Contract w s e (TxOutRef, TxOutTx, ScriptInstance PubKeyContract)
pubKeyContract pk vl = mapError (review _PubKeyError) $ do
  let inst = scriptInstance pk
      address = Scripts.scriptAddress inst
      tx = Constraints.mustPayToTheScript () vl

  ledgerTx <- submitTxConstraints inst tx

  _ <- awaitTxConfirmed (txId ledgerTx)
  let output =
        Map.toList $
          Map.filter ((==) address . txOutAddress) $
            unspentOutputsTx ledgerTx
  case output of
    [] -> throwing _ScriptOutputMissing pk
    [(outRef, outTxOut)] -> pure (outRef, TxOutTx {txOutTxTx = ledgerTx, txOutTxOut = outTxOut}, inst)
    _ -> throwing _MultipleScriptOutputs pk
