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
import Ledger.Value (Value, geq) -- , lt)
import Prelude
import qualified Ledger as L
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract qualified as PC


--Needed for redeem
import Control.Lens (makeClassyPrisms, review, view)
import Control.Monad.Error.Lens (throwing)
import Plutus.Contract.Typed.Tx qualified as Typed
import Ledger.Interval qualified as Interval
import Ledger.Constraints qualified as Constraints
import Ledger.Tx qualified as Tx
import Ledger.Value (Value, geq, lt)

--Needed for refund
import PlutusTx qualified
import Plutus.Script.Utils.V1.Scripts (datumHash)





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
    unspentOutputs <- scriptUtxosSuchThat inst (\_ x -> True)
    current <- currentTime
    let 
      addr = Scripts.validatorAddress inst
      valRange = Interval.to (pred $ escrowDeadline escrow)
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
                                  ScriptTarget vh d vl -> error "can't use script with cooked")  
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
    pk <- ownFirstPaymentPubKeyHash
    unspentOutputs <- scriptUtxosSuchThat inst (\_ x -> True)
    current <- currentTime
    let 
      uouts = refundFilter pk (map fst unspentOutputs)
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

-- Todo define payReddemRefund















{-
txOffer :: MonadBlockChain m => L.Value -> Integer -> m SpendableOut
txOffer lot minBid = do
  seller <- ownPaymentPubKeyHash
  tx <-
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True}) $
        paysScript A.auctionValidator (A.Offer seller minBid) lot
  outputs <- spOutsFromCardanoTx tx
  -- the transaction created exactly one script output, so the call to head never fail
  return $ head $ filter (isJust . sBelongsToScript) outputs
-}

{-
tx = Typed.collectFromScript unspentOutputs Redeem
--                <> foldMap mkTx (escrowTargets escrow)
--                <> Constraints.mustValidateIn valRange
-}


{-
redeem inst escrow = mapError (review _EscrowError) $ do
    let addr = Scripts.validatorAddress inst
    current <- currentTime
    unspentOutputs <- utxosAt addr
    let
        valRange = Interval.to (Haskell.pred $ escrowDeadline escrow)
        tx = Typed.collectFromScript unspentOutputs Redeem
                <> foldMap mkTx (escrowTargets escrow)
                <> Constraints.mustValidateIn valRange
    if current >= escrowDeadline escrow
    then throwing _RedeemFailed DeadlinePassed
    else if foldMap (view Tx.ciTxOutValue) unspentOutputs `lt` targetTotal escrow
-}




{-
-- This is useful when writing endpoints and/or traces to fetch utxos of
-- interest right from the start and avoid querying the chain for them
-- afterwards using "utxosSuchThat" functions.
spOutsFromCardanoTx :: MonadBlockChain m => Pl.CardanoTx -> m [SpendableOut]
spOutsFromCardanoTx cardanoTx = forM (Pl.getCardanoTxOutRefs cardanoTx) $
  \(txOut, txOutRef) ->
    case Pl.fromTxOut txOut of
      Just chainIndexTxOut -> spOutResolveDatum (txOutRef, chainIndexTxOut)
      Nothing -> fail "could not extract ChainIndexTxOut"
-}
    

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




                    -- [paysPK addr $ map (sOutValue . fst) (escrowTargets escrow)]

    -- we probably want something like case utxos of 

    
    {- let
        txs = PC.collectFromScript L.unspentOutputs Redeem 
                    <> foldMap mkTx (escrowTargets escrow) 
                    <> Constraints.mustValidateIn valRange -}
    {-
    tx <- 
        validateTxSkel $ 
            txSkelOpts (def {adjustUnbalTx = True}) $ 
                foldMap paysPK (escrowTargets escrow) -}






{-
redeem2 ::
    MonadBlockChain m 
    => Pl.TypedValidator Escrow
    -> EscrowParams Datum 
    -> m L.CardanoTx
redeem2 inst escrow = PC.mapError (review _EscrowError) $ do
    let addr = Scripts.validatorAddress inst
    current <- currentTime
    unspentOutputs <- PC.utxosAt addr
    let
        valRange = Interval.to (pred $ escrowDeadline escrow)
        tx = Typed.collectFromScript unspentOutputs Redeem
                <> foldMap mkTx (escrowTargets escrow)
                <> Constraints.mustValidateIn valRange
    if current >= escrowDeadline escrow
    then throwing _RedeemFailed DeadlinePassed
    else if foldMap (view Tx.ciTxOutValue) unspentOutputs `lt` targetTotal escrow
         then throwing _RedeemFailed NotEnoughFundsAtAddress
         else do
            tx <- 
                validateTxSkel $ 
                    txSkelOpts (def {adjustUnbalTx = True}) $ 
                        foldMap paysPK (escrowTargets escrow)
            return tx
-}          


           {-utx <- PC.mkTxConstraints ( Constraints.typedValidatorLookups inst
                                 <> Constraints.unspentOutputs unspentOutputs
                                  ) tx
           adjusted <- PC.adjustUnbalancedTx utx
           RedeemSuccess . Tx.getCardanoTxId <$> PC.submitUnbalancedTx adjusted -}

{-
redeem inst escrow = do 
    let addr = Scripts.validatorAddress inst
    unspentOutputs <- PC.utxosAt addr

    -- we probably want something like case utxos of 

    case unspentOutputs of 


    
    {- let
        txs = PC.collectFromScript L.unspentOutputs Redeem 
                    <> foldMap mkTx (escrowTargets escrow) 
                    <> Constraints.mustValidateIn valRange -}
    
    tx <- 
        validateTxSkel $ 
            txSkelOpts (def {adjustUnbalTx = True}) $ 
                foldMap paysPK (escrowTargets escrow)

  return tx
-}


{-
redeem ::
    MonadBlockChain m 
    => Pl.TypedValidator Escrow
    -> EscrowParams Datum 
    -> m L.CardanoTx
redeem inst escrow = do 
    let addr = Scripts.validatorAddress inst
    --unspentOutputs <- utxosAt addr
    unspentOutputs <- scriptUtxosSuchThat inst (\_ x -> True)

    -- we probably want something like case utxos of 

    case unspentOutputs of 


    
    {- let
        txs = PC.collectFromScript L.unspentOutputs Redeem 
                    <> foldMap mkTx (escrowTargets escrow) 
                    <> Constraints.mustValidateIn valRange -}
    
    tx <- 
        validateTxSkel $ 
            txSkelOpts (def {adjustUnbalTx = True}) $ 
                foldMap paysPK (escrowTargets escrow)
    return tx
-}


{-
redeem ::
    MonadBlockChain m 
    => TypedValidator Escrow
    -> EscrowParams Datum 
    -> m RedeemSuccess
redeem inst escrow = mapError (review _EscrowError) $ do
    let addr = Scripts.validatorAddress inst
    current <- currentTime
    unspentOutputs <- utxosAt addr
    let
        valRange = Interval.to (Haskell.pred $ escrowDeadline escrow)
--        tx = Typed.collectFromScript unspentOutputs Redeem
--                <> foldMap mkTx (escrowTargets escrow)
--                <> Constraints.mustValidateIn valRange
    if current >= escrowDeadline escrow
    then throwing _RedeemFailed DeadlinePassed
    else if foldMap (view Tx.ciTxOutValue) unspentOutputs `lt` targetTotal escrow
-}


{-
  tx <-
    validateTxSkel $
      txSkelOpts (def {adjustUnbalTx = True}) $
        paysScript A.auctionValidator (A.Offer seller minBid) lot
-}

{-
    else if foldMap (view Tx.ciTxOutValue) unspentOutputs `lt` targetTotal escrow
         then throwing _RedeemFailed NotEnoughFundsAtAddress
         else do
           utx <- mkTxConstraints ( Constraints.typedValidatorLookups inst
                                 <> Constraints.unspentOutputs unspentOutputs
                                  ) tx
           adjusted <- adjustUnbalancedTx utx
           RedeemSuccess . getCardanoTxId <$> submitUnbalancedTx adjusted



-}


{-
-- | Redeem all outputs at the contract address using a transaction that
--   has all the outputs defined in the contract's list of targets.
redeem ::
    forall w s e.
    ( AsEscrowError e
    )
    => TypedValidator Escrow
    -> EscrowParams Datum
    -> Contract w s e RedeemSuccess
redeem inst escrow = mapError (review _EscrowError) $ do
    let addr = Scripts.validatorAddress inst
    current <- currentTime
    unspentOutputs <- utxosAt addr
    let
        valRange = Interval.to (Haskell.pred $ escrowDeadline escrow)
        tx = Typed.collectFromScript unspentOutputs Redeem
                <> foldMap mkTx (escrowTargets escrow)
                <> Constraints.mustValidateIn valRange
    if current >= escrowDeadline escrow
    then throwing _RedeemFailed DeadlinePassed
    else if foldMap (view Tx.ciTxOutValue) unspentOutputs `lt` targetTotal escrow
         then throwing _RedeemFailed NotEnoughFundsAtAddress
         else do
           utx <- mkTxConstraints ( Constraints.typedValidatorLookups inst
                                 <> Constraints.unspentOutputs unspentOutputs
                                  ) tx
           adjusted <- adjustUnbalancedTx utx
           RedeemSuccess . getCardanoTxId <$> submitUnbalancedTx adjusted
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