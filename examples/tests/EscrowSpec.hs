{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE ImportQualifiedPost #-}

module EscrowSpec where

-- import qualified Escrow as E
-- import qualified Escrow.OffChain as E
{-
import Control.Lens hiding (both)
import Control.Monad (void, when)
import Data.Data
import Data.Default (Default (def))
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map

import Ledger (Slot (..), minAdaTxOut)
import Ledger.Ada qualified as Ada
import Ledger.Time (POSIXTime)
import Ledger.TimeSlot qualified as TimeSlot
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value
import Plutus.Contract hiding (currentSlot)
import Plutus.Contract.Test
import Plutus.Contract.Test.ContractModel

import Plutus.Trace.Emulator qualified as Trace
import PlutusTx.Monoid (inv)

import Test.QuickCheck as QC hiding ((.&&.))
import Test.Tasty
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck hiding ((.&&.))
-}

import Ledger.Time (POSIXTime)
import Ledger.TimeSlot qualified as TimeSlot

import Plutus.Contract.Test qualified as PTest


import Escrow
import Escrow.OffChain
import Control.Applicative
import Control.Arrow
import Control.Monad
import Cooked
import Data.Default
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Ledger as L
import qualified Ledger.Ada as Ada
import qualified Ledger.Value as Pl
import qualified Ledger.Value as Value
import Optics.Core
import qualified Plutus.Script.Utils.V1.Scripts as Pl
import qualified PlutusTx.Numeric as Pl
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

-- | initial distribution s.t. everyone owns five bananas
testInit :: InitialDistribution
testInit = initialDistribution' [(i, [minAda]) | i <- knownWallets]

escrowParams :: POSIXTime -> EscrowParams d
escrowParams startTime =
  EscrowParams
    { escrowDeadline = startTime + 40000
    , escrowTargets  =
        [ payToPaymentPubKeyTarget (PTest.mockWalletPaymentPubKeyHash PTest.w1) (Ada.adaValueOf 10)
        , payToPaymentPubKeyTarget (PTest.mockWalletPaymentPubKeyHash PTest.w2) (Ada.adaValueOf 20)
        ]
    }

-- typedValidator

usageExample :: Assertion
usageExample = testSucceeds $ do
    pay (typedValidator (escrowParams (TimeSlot.scSlotZeroTime def))) 
        (escrowParams (TimeSlot.scSlotZeroTime def)) 
        (Ada.adaValueOf 30) `as` wallet 1


redeemTrace :: MonadMockChain m => m RedeemSuccess
redeemTrace = do
    let
        val = (typedValidator (escrowParams (TimeSlot.scSlotZeroTime def)))
        params = (escrowParams (TimeSlot.scSlotZeroTime def)) 
    pay val params (Ada.adaValueOf 20) `as` wallet 1
    pay val params (Ada.adaValueOf 10) `as` wallet 2
    redeem val params `as` wallet 3

redeem2Trace :: MonadMockChain m => m RedeemSuccess
redeem2Trace = do
    let
        val = (typedValidator (escrowParams (TimeSlot.scSlotZeroTime def)))
        params = (escrowParams (TimeSlot.scSlotZeroTime def))
    pay val params (Ada.adaValueOf 20) `as` wallet 1
    pay val params (Ada.adaValueOf 10) `as` wallet 2
    pay val params (Ada.adaValueOf 10) `as` wallet 3
    redeem val params `as` wallet 1

refundTrace :: MonadMockChain m => m RefundSuccess
refundTrace = do
    t0 <- currentTime
    let
        val = (typedValidator (escrowParams (TimeSlot.scSlotZeroTime def)))
        params = (escrowParams (TimeSlot.scSlotZeroTime def))
        deadline = t0 + 60_000
    pay val params (Ada.adaValueOf 20) `as` wallet 1
    awaitTime deadline
    refund val params `as` wallet 1

-- | helper function to compute what the given wallet owns in the
-- given state
holdingInState :: UtxoState -> Wallet -> L.Value
holdingInState (UtxoState m) w
  | Just vs <- M.lookup (walletAddress w) m = utxoValueSetTotal vs
  | otherwise = mempty

holdingInState2 :: InitialDistribution -> Wallet -> L.Value
holdingInState2 d w = mconcat (valuesForWallet d w)

-- Terrible way to do this but just using this to compare values assuming some fee has been applied
naiveValueComparison :: L.Value -> L.Value -> Bool
naiveValueComparison v1 v2 = if (Ada.fromValue v1) >= (Ada.fromValue v2)
                        && (floor $ (* 0.999) $ fromIntegral (Ada.getLovelace (Ada.fromValue v1)))
                                <= (Ada.getLovelace (Ada.fromValue v2))
                                        then True else False

tests :: TestTree
tests = 
    testGroup
        "EscrowSpec"
            [ testCase "Simple example succeeds" usageExample,
              testCase "Can redeem"
                $ testSucceeds
                    (allowBigTransactions redeemTrace),
              testCase "Check wallets and can redeem"
                $ testSucceedsFrom'
                    ( \_ s ->
                       -- testBool $ (Ada.fromValue ((holdingInState2 testInit (wallet 2)) <> (Ada.adaValueOf 10))
                         --           == Ada.fromValue (holdingInState s (wallet 2)))
                        -- testBool $ (Ada.fromValue ((holdingInState2 testInit (wallet 3)))
                         --           == Ada.fromValue (holdingInState s (wallet 3)))
                        testBool $ naiveValueComparison
                                        ((holdingInState2 testInit (wallet 2))
                                                <> (Ada.adaValueOf 10))
                                        (holdingInState s (wallet 2))
                        .&&. (testBool $ naiveValueComparison
                                           ((holdingInState2 testInit (wallet 1))
                                             <> (Ada.adaValueOf (-10)))
                                           (holdingInState s (wallet 1)))
                        -- .&&. (testBool $ naiveValueComparison
                        --                   (holdingInState2 testInit (wallet 3))
                        --                   (holdingInState s (wallet 3)))

                    )                    
                    testInit  
                    (allowBigTransactions redeemTrace),
              testCase "can redeem even if more money than required has been paid in"
                $ testSucceedsFrom'
                    ( \_ s ->
                        testBool $ naiveValueComparison
                                           ((holdingInState2 testInit (wallet 2))
                                             <> (Ada.adaValueOf 10))
                                           (holdingInState s (wallet 2))
                        .&&. (testBool $ naiveValueComparison
                                           ((holdingInState2 testInit (wallet 3))
                                             <> (Ada.adaValueOf (-10)))
                                           (holdingInState s (wallet 3)))
                    )
                    testInit
                    (allowBigTransactions redeem2Trace),
              testCase "Can refund"
                $ testSucceeds
                    (allowBigTransactions refundTrace)
            ]


{-
tests :: TestTree
tests = testGroup "escrow"
    [ let con = void $ payEp @() @EscrowSchema @EscrowError (escrowParams startTime) in
      checkPredicateOptions options "can pay"
        ( assertDone con (Trace.walletInstanceTag w1) (const True) "escrow pay not done"
        .&&. walletFundsChange w1 (Ada.adaValueOf (-10))
        )
        $ do
          hdl <- Trace.activateContractWallet w1 con
          Trace.callEndpoint @"pay-escrow" hdl (Ada.adaValueOf 10)
          void $ Trace.waitNSlots 1

    , let con = void $ selectEither (payEp @()
                                           @EscrowSchema
                                           @EscrowError
                                           (escrowParams startTime))
                                    (redeemEp (escrowParams startTime)) in
      checkPredicateOptions options "can redeem"
        ( assertDone con (Trace.walletInstanceTag w3) (const True) "escrow redeem not done"
          .&&. walletFundsChange w1 (Ada.adaValueOf (-10))
          .&&. walletFundsChange w2 (Ada.adaValueOf 10)
          .&&. walletFundsChange w3 mempty
        )
        redeemTrace

-- | Wallets 1 and 2 pay into an escrow contract, wallet 3
--   cashes out.
redeemTrace :: Trace.EmulatorTrace ()
redeemTrace = do
    startTime <- TimeSlot.scSlotZeroTime <$> Trace.getSlotConfig
    let con = void $ selectEither (payEp @()
                                         @EscrowSchema
                                         @EscrowError
                                         (escrowParams startTime))
                                  (redeemEp (escrowParams startTime))
    hdl1 <- Trace.activateContractWallet w1 con
    hdl2 <- Trace.activateContractWallet w2 con
    hdl3 <- Trace.activateContractWallet w3 con

    Trace.callEndpoint @"pay-escrow" hdl1 (Ada.adaValueOf 20)
    Trace.callEndpoint @"pay-escrow" hdl2 (Ada.adaValueOf 10)
    _ <- Trace.waitNSlots 1
    Trace.callEndpoint @"redeem-escrow" hdl3 ()
    void $ Trace.waitNSlots 1
-}
