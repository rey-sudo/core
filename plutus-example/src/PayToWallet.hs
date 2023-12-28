{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE TypeApplications   #-}

module PayToWallet(
    payToWallet
    , PayToWalletParams(..)
    , PayToWalletSchema
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Void (Void)
import GHC.Generics (Generic)

import Ledger (PaymentPubKeyHash (PaymentPubKeyHash, unPaymentPubKeyHash),)
import Ledger.Tx.Constraints (mustPayToPubKey)
import Plutus.Contract (ContractError, Endpoint, Promise, adjustUnbalancedTx, endpoint, logInfo, mkTxConstraints,
                        yieldUnbalancedTx)
import Plutus.Script.Utils.Value (Value)
import Plutus.Script.Utils.Ada qualified as Ada
import Utilities 

data PayToWalletParams =
    PayToWalletParams
        { pPriceParam     :: Integer
        , sWalletParam    :: String
        }
        deriving stock (Eq, Show, Generic)
        deriving anyclass (ToJSON, FromJSON)

type PayToWalletSchema = Endpoint "PayToWallet" PayToWalletParams

payToWallet :: Promise () PayToWalletSchema ContractError ()
payToWallet = endpoint @"PayToWallet" $ \PayToWalletParams{pPriceParam, sWalletParam} -> do
    logInfo @String "Calling PayToWallet endpoint"
    let price = Ada.toValue $ Ada.lovelaceOf pPriceParam
        ppkh  = bStoPPKH sWalletParam
    utx <- mkTxConstraints @Void mempty (mustPayToPubKey ppkh price)
    logInfo @String $ "Yielding the unbalanced transaction " <> show utx
    adjustUnbalancedTx utx >>= yieldUnbalancedTx



bStoPPKH :: String -> PaymentPubKeyHash
bStoPPKH bs = PaymentPubKeyHash $ pkhToPubKeyHash bs

