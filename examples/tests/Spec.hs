module Main where

import qualified AuctionSpec
import qualified PMultiSigStatefulSpec
import qualified SplitSpec
import qualified SplitUPLCSpec
import Test.Tasty
import qualified UseCaseCrowdfundingSpec
import qualified EscrowSpec
import qualified EscrowSpecOld


main :: IO ()
main = do
  defaultMain $
    testGroup
      "main"
      [ -- PMultiSigStatefulSpec.tests,
        -- UseCaseCrowdfundingSpec.tests,
        -- SplitSpec.tests,
        -- SplitUPLCSpec.tests,
        -- AuctionSpec.tests
        -- EscrowSpec.tests
        EscrowSpecOld.tests
      ]
