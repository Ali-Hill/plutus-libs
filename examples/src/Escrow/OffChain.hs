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


import Control.Lens (review, view) -- makeClassyPrisms , view)
import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import qualified Ledger as Pl
-- import qualified Ledger.Ada as Pl
import qualified Ledger.Typed.Scripts as Pl
import Ledger.Value (Value, lt) -- geq , lt)
import Prelude
import qualified Ledger as L
import Plutus.Contract qualified as PC
import Ledger.Tx qualified as Tx
import PlutusTx qualified
import Plutus.Script.Utils.V1.Scripts (datumHash)
import Data.Default
import Plutus.V1.Ledger.Scripts (Datum (Datum)) -- DatumHash, ValidatorHash)

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
    pk <- ownPaymentPubKeyHash 
    (validateTxSkel $
          txSkelOpts (def {adjustUnbalTx = True}) $
        [ValidateIn deadline]
            :=>:
                [paysScript
                    inst
                    (L.PaymentPubKeyHash pk)
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


newtype RedeemSuccess = RedeemSuccess L.TxId
    deriving (Eq, Show)


redeemEp ::
    forall w s e.
    ( PC.HasEndpoint "redeem-escrow" () s
    , AsEscrowError e
    )
    => EscrowParams Datum
    -> PC.Promise w s e RedeemSuccess
redeemEp escrow = PC.promiseMap
    (PC.mapError (review _EscrowError))
    (PC.endpoint @"redeem-escrow" $ \() -> redeem (typedValidator escrow) escrow)

redeem ::
    MonadBlockChain m 
    => Pl.TypedValidator Escrow
    -> EscrowParams Datum 
    -> m RedeemSuccess
redeem inst escrow = do
    unspentOutputs <- scriptUtxosSuchThat inst (\_ _ -> True)
    current <- currentTime
    let 
      uouts = map snd (map fst unspentOutputs)
      deadline = L.interval 1 (escrowDeadline escrow)
    if current >= escrowDeadline escrow
    then error "Deadline Passed"
    else if foldMap (view Tx.ciTxOutValue) uouts `lt` targetTotal escrow
      then error "Not enough funds at address"
      else do
        tx <- 
          validateTxSkel $ 
              txSkelOpts (def {adjustUnbalTx = True}) $ 
                  (ValidateIn deadline
                    : map (SpendsScript inst Redeem . fst) unspentOutputs)
                      :=>: map (\case 
                                  PaymentPubKeyTarget pk vl -> paysPK (L.unPaymentPubKeyHash pk) vl
                                  ScriptTarget _ _ _ -> error "can't use script with cooked")  
                                  (escrowTargets escrow)
        return (RedeemSuccess (L.getCardanoTxId tx))

newtype RefundSuccess = RefundSuccess L.TxId
    deriving (Eq, Show)

-- | 'refund' with an endpoint.
refundEp ::
    forall w s.
    ( PC.HasEndpoint "refund-escrow" () s
    )
    => EscrowParams Datum
    -> PC.Promise w s EscrowError RefundSuccess
refundEp escrow = PC.endpoint @"refund-escrow" $ \() -> refund (typedValidator escrow) escrow

refundFilter :: L.PaymentPubKeyHash -> [SpendableOut] -> [SpendableOut]
refundFilter pk sout = 
    let 
      flt (_ , ciTxOut) = either id datumHash (Tx._ciTxOutDatum ciTxOut) == datumHash (Datum (PlutusTx.toBuiltinData pk))
    in 
      (filter flt sout)

refund ::
    MonadBlockChain m 
    => Pl.TypedValidator Escrow
    -> EscrowParams Datum 
    -> m RefundSuccess
refund inst escrow = do
    pk <- ownPaymentPubKeyHash
    unspentOutputs <- scriptUtxosSuchThat inst (\_ _ -> True)
    current <- currentTime
    let 
      uouts = refundFilter (L.PaymentPubKeyHash pk) (map fst unspentOutputs)
    tx <- validateTxSkel $ 
              txSkelOpts (def {adjustUnbalTx = True}) $ 
                  (After (escrowDeadline escrow)
                    : map (SpendsScript inst Refund) uouts)
    if current <= escrowDeadline escrow
    then error "refund before deadline"
    else if (map (SpendsScript inst Refund) uouts) == []
    then error "no scripts to refund"
    else 
      return (RefundSuccess (L.getCardanoTxId tx)) 

-- Todo define payRedeemRefund

{-
txRefund :: MonadBlockChain m => m ()
txRefund = do
  funder <- ownPaymentPubKeyHash
  utxos <-
    scriptUtxosSuchThat Cf.crowdfundingValidator (\d _ -> Cf.getFunder d == Just funder)
  void $
    validateTxSkel $
      txSkel $
        map (SpendsScript Cf.crowdfundingValidator Cf.IndividualRefund . fst) utxos
          :=>: [paysPK funder (PlutusTx.Prelude.sum $ map (sOutValue . fst) utxos)]
-}
