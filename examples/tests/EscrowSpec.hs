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

import Plutus.Contract.Test


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


escrowParams :: POSIXTime -> EscrowParams d
escrowParams startTime =
  EscrowParams
    { escrowDeadline = startTime + 40000
    , escrowTargets  =
        [ payToPaymentPubKeyTarget (mockWalletPaymentPubKeyHash w1) (Ada.adaValueOf 10)
        , payToPaymentPubKeyTarget (mockWalletPaymentPubKeyHash w2) (Ada.adaValueOf 20)
        ]
    }
-- typedValidator

usageExample :: Assertion
usageExample = testSucceeds $ do
				pay (typedValidator (escrowParams (TimeSlot.scSlotZeroTime def))) (escrowParams (TimeSlot.scSlotZeroTime def)) (Ada.adaValueOf 10)

tests :: TestTree
tests = 
	testGroup
		"EscrowSpec"
		[ testCase "Simple example succeeds" usageExample ]
