{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# LANGUAGE ImportQualifiedPost  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Governance.OffChain where

import Control.Monad
import Cooked.MockChain
import Cooked.Tx.Constraints
import qualified Ledger as Pl
import qualified Ledger.Ada as Pl
import qualified Ledger.Typed.Scripts as Pl

import Governance

-- | The main contract for creating a new law and for voting on proposals.

{-
contract ::
    AsGovError e
    => Params
    -> Contract () Schema e ()   
contract params = forever $ mapError (review _GovError) endpoints where
    theClient = client params
    endpoints = selectList [initLaw, addVote]

    addVote = endpoint @"add-vote" $ \(tokenName, vote) ->
        void $ SM.runStep theClient (AddVote tokenName vote)

    initLaw = endpoint @"new-law" $ \bsLaw -> do
        let mph = Scripts.forwardingMintingPolicyHash (typedValidator params)
        void $ SM.runInitialise theClient (GovState (toBuiltin bsLaw) mph Nothing) (Ada.lovelaceValueOf 1)
        let tokens = Haskell.zipWith (const (mkTokenName (baseTokenName params))) (initialHolders params) [1..]
        void $ SM.runStep theClient $ MintTokens tokens
-}


contract2 