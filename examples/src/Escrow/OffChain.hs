{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImportQualifiedPost  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}
{-# OPTIONS_GHC -g -fplugin-opt PlutusTx.Plugin:coverage-all #-}
-- | A general-purpose escrow contract in Plutus
module Escrow.OffChain where


import Control.Lens (makeClassyPrisms, review) -- , view)
import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import qualified Ledger as Pl
import qualified Ledger.Ada as Pl
import qualified Ledger.Typed.Scripts as Pl
import Plutus.Contract qualified as PC
import Ledger.Value (Value, geq) -- , lt)
import Prelude
import qualified Ledger as L

import Data.Default



import Plutus.V1.Ledger.Scripts (Datum (Datum), DatumHash, ValidatorHash)

import Escrow

{-
escrowContract :: MonadBlockChain m => Pl.TypedValidator Escrow -> EscrowParams Datum -> m () 
escrowContract v escrow = 
    let inst = typedValidator escrow
        payAndRefund = endpoint @"pay-escrow" $ \vl -> do
            _ <- pay inst escrow vl
            _ <- awaitTime $ escrowDeadline escrow
            refund inst escrow
    in selectList
        [ void payAndRefund
        , void $ redeemEp escrow
        ]
-}


-- this should be fine once we define pay 
payEp ::
    forall w s e.
    ( PC.HasEndpoint "pay-escrow" Value s
    , AsEscrowError e
    )
    => EscrowParams Datum
    -> PC.Promise w s e Pl.TxId
payEp escrow = PC.promiseMap
    (PC.mapError (review _EContractError))
    (PC.endpoint @"pay-escrow" $ pay (typedValidator escrow) escrow)


{-
-- | Pay some money into the escrow contract.
pay ::
    forall w s e.
    ( AsContractError e
    )
    => TypedValidator Escrow
    -- ^ The instance
    -> EscrowParams Datum
    -- ^ The escrow contract
    -> Value
    -- ^ How much money to pay in
    -> Contract w s e TxId
pay inst escrow vl = do
    pk <- ownFirstPaymentPubKeyHash
    let tx = Constraints.mustPayToTheScript pk vl
          <> Constraints.mustValidateIn (Ledger.interval 1 (escrowDeadline escrow))
    mkTxConstraints (Constraints.typedValidatorLookups inst) tx
        >>= adjustUnbalancedTx
        >>= submitUnbalancedTx
        >>= return . getCardanoTxId
-}

pay ::
    MonadBlockChain m
    => Pl.TypedValidator Escrow
    -- ^ The instance
    -> EscrowParams Datum
    -- ^ The escrow contract
    -> Value
    -- ^ How much money to pay in
    -> m L.TxId
pay inst escrow vl = do
    let deadline = L.interval 1 (escrowDeadline escrow)
    pk <- ownFirstPaymentPubKeyHash 
    (validateTxSkel $
          txSkelOpts (def {adjustUnbalTx = True}) $
        [ValidateIn deadline]
            :=>:
                [paysScript
                    inst
                    pk
                    vl
                ]) >>= return . L.getCardanoTxId

{-
    let tx = Constraints.mustPayToTheScript pk vl
          <> Constraints.mustValidateIn (Ledger.interval 1 (escrowDeadline escrow))
    mkTxConstraints (Constraints.typedValidatorLookups inst) tx
        >>= adjustUnbalancedTx
        >>= submitUnbalancedTx
        >>= return . getCardanoTxId
-}

{-
https://playground.plutus.iohkdev.io/doc/haddock/plutus-contract/html/Plutus-Contract.html

mkTxConstraints :: forall a w s e. 
    (ToData (RedeemerType a), FromData (DatumType a), 
    ToData (DatumType a), AsContractError e) 
        => 
            ScriptLookups a 
            -> TxConstraints (RedeemerType a) (DatumType a) 
            -> Contract w s e UnbalancedTxSource#

Build a transaction that satisfies the constraints


https://github.com/tweag/plutus-libs/tree/main/cooked-validators#a-quick-example
https://plutus-apps.readthedocs.io/en/latest/plutus/tutorials/basic-apps.html#defining-the-validator-script
https://github.com/tweag/plutus-libs/blob/main/cooked-validators/src/Cooked/MockChain/Monad/Staged.hs
-}


{-
escrowContract :: MonadBlockChain m => EscrowParams Datum -> m () 
escrowContract escrow = 
    let inst = typedValidator escrow
        payAndRefund = endpoint @"pay-escrow" $ \vl -> do
            _ <- _ -- pay inst escrow vl
            _ <- _ -- awaitTime $ escrowDeadline escrow
            refund inst escrow
    in selectList
        [ void payAndRefund
        , void $ redeemEp escrow
        ]
-}


{-
txLock script datum =
  void $
    validateTxConstrLbl
      (TxLock datum)
      [ paysScript
          script
          datum
          (Pl.lovelaceValueOf (Split.amount datum))
      ]

txLock :: MonadBlockChain m => Pl.TypedValidator Split -> SplitDatum -> m ()

lockFunds :: SplitData -> Contract () SplitSchema T.Text ()
lockFunds s@SplitData{amount} = do
    logInfo $ "Locking " <> Haskell.show amount
    let tx = Constraints.mustPayToTheScriptWithDatumInTx s (Ada.toValue amount)
    void $ submitTxConstraints splitValidator tx
-}

{-
escrowContract :: MonadBlockChain m => Pl.TypedValidator Escrow -> EscrowParams Datum -> m () 
escrowContract v escrow = 
    let inst = typedValidator escrow
        payAndRefund = endpoint @"pay-escrow" $ \vl -> do
            _ <- pay inst escrow vl
            _ <- awaitTime $ escrowDeadline escrow
            refund inst escrow
    in selectList
        [ void payAndRefund
        , void $ redeemEp escrow
        ]
-}


{-
escrowContract
    :: EscrowParams Datum
    -> Contract () EscrowSchema EscrowError ()
escrowContract escrow =
    let inst = typedValidator escrow
        payAndRefund = endpoint @"pay-escrow" $ \vl -> do
            _ <- pay inst escrow vl
            _ <- awaitTime $ escrowDeadline escrow
            refund inst escrow
    in selectList
        [ void payAndRefund
        , void $ redeemEp escrow
        ]
-}