{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE TypeApplications   #-}


{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}


module PayToWallet(
      StartParams(..)
    , SlaveSchema
    , contract
    , startEndpoint
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Void (Void)
import GHC.Generics (Generic)

import Plutus.Contract (ContractError, type (.\/), Endpoint, Contract, Promise, AsContractError(..), endpoint, logInfo, logWarn, ownFirstPaymentPubKeyHash, utxosAt, selectList)
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash, unPaymentPubKeyHash),)
import Ledger.Tx.Constraints (mustPayToPubKey)
import Plutus.Contract (mapError, ownUtxos, ContractError, Endpoint, Promise, adjustUnbalancedTx, endpoint, logInfo, mkTxConstraints,
                        yieldUnbalancedTx)
import Plutus.Script.Utils.Value (Value)
import Plutus.Script.Utils.Ada qualified as Ada
import Utilities 
import Prelude qualified as Haskell
import PlutusTx.Prelude hiding (Applicative (..), check)
import Control.Monad (forever, void)
import Ledger.Tx.Constraints qualified as Constraints
import Utilities (pkhToPubKeyHash)
import Control.Lens (_2, makeClassyPrisms, review, (^?))
import PlutusTx qualified
import PlutusTx.Monoid (inv)
import Control.Monad.Error.Lens
import Data.Void (Void, absurd)

data StartParams =
    StartParams
        { pPriceParam     :: Haskell.Integer
        , sWalletParam    :: Haskell.String
        }
        deriving stock (Haskell.Eq, Haskell.Show, Generic)
        deriving anyclass (ToJSON, FromJSON)

type SlaveSchema = Endpoint "Start" StartParams



contract :: Contract () SlaveSchema ContractError ()
contract = forever endpoints where
        endpoints      = selectList [startEndpoint]


startEndpoint :: Promise () SlaveSchema ContractError ()
startEndpoint = endpoint @"Start" $ \StartParams{pPriceParam, sWalletParam} -> 
    mapError (review _ContractError) $ do
    logInfo @Haskell.String "Calling PayToWallet endpoint"
    let price = Ada.toValue $ Ada.lovelaceOf pPriceParam
        ppkh  = bStoPPKH sWalletParam
        constraints = mustPayToPubKey ppkh price

    logInfo @Haskell.String "testi"

    utx <- mkTxConstraints @Void Haskell.mempty (mustPayToPubKey ppkh price)
    logInfo @Haskell.String $ "Yielding the unbalanced transaction " <> Haskell.show utx
    adjustUnbalancedTx utx >>= yieldUnbalancedTx


--utxosAt $ ppkhToAddress $ bStoPPKH sWalletParam

bStoPPKH :: Haskell.String -> PaymentPubKeyHash
bStoPPKH bs = PaymentPubKeyHash $ pkhToPubKeyHash bs


