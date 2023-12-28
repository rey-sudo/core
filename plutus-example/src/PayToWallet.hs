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


import Ledger (PaymentPubKeyHash (PaymentPubKeyHash, unPaymentPubKeyHash),)
import Ledger.Address (CardanoAddress(..), PaymentPrivateKey (PaymentPrivateKey, unPaymentPrivateKey),
                       PaymentPubKey (PaymentPubKey, unPaymentPubKey),
                       PaymentPubKeyHash (PaymentPubKeyHash, unPaymentPubKeyHash),
                       StakePubKey (StakePubKey, unStakePubKey), StakePubKeyHash (StakePubKeyHash, unStakePubKeyHash),
                       stakePubKeyHashCredential)

import Ledger.Tx.Constraints (mustPayToPubKey)
import Plutus.Contract (ContractError, type (.\/), Endpoint, Contract, Promise, AsContractError(..), endpoint, logInfo, logWarn, ownFirstPaymentPubKeyHash, utxosAt, selectList)
import Plutus.Contract (mapError, ownUtxos, ContractError, Endpoint, Promise, adjustUnbalancedTx, endpoint, logInfo, mkTxConstraints,
                        yieldUnbalancedTx)
import Plutus.Contract.Request (utxosAt)                        
import Plutus.Script.Utils.Value (Value)
import Plutus.Script.Utils.Ada qualified as Ada
import Utilities 
import Prelude qualified as Haskell
import PlutusTx.Prelude hiding (Applicative (..), check)
import Control.Monad (forever, void)
import Ledger.Tx.Constraints.OffChain qualified as Constraints
import Utilities (pkhToPubKeyHash)
import Control.Lens (_2, makeClassyPrisms, review, (^?))
import PlutusTx qualified
import PlutusTx.Monoid (inv)
import Control.Monad.Error.Lens
import Data.Void (Void, absurd)
import Cardano.Node.Emulator.Internal.Node.Params qualified as Nparams
import Ledger.Tx.CardanoAPI.Internal qualified as Tx
import Plutus.V1.Ledger.Api (Address (Address), Credential (PubKeyCredential), StakingCredential (StakingHash))


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
    logInfo @Haskell.String "1"

    utxos <- utxosAt $ ppkhToAddress (bStoPPKH sWalletParam )

    logInfo @Haskell.String $ "!!!!!!ownUtxos!!!" <> Haskell.show utxos
    
    

    let price = Ada.toValue $ Ada.lovelaceOf pPriceParam
        ppkh  = bStoPPKH sWalletParam
        constraints = (mustPayToPubKey ppkh price)
        lookups = (Constraints.unspentOutputs utxos)
    

    logInfo @Haskell.String "testi"

    utx <- mkTxConstraints @Void lookups constraints
    logInfo @Haskell.String $ "Yielding the unbalanced transaction " <> Haskell.show utx
    adjustUnbalancedTx utx >>= yieldUnbalancedTx


--utxosAt $ ppkhToAddress $ bStoPPKH sWalletParam

bStoPPKH :: Haskell.String -> PaymentPubKeyHash
bStoPPKH bs = PaymentPubKeyHash $ pkhToPubKeyHash bs

getRight :: Haskell.Either a b -> b
getRight (Haskell.Right x) = x
getRight (Haskell.Left _)  = Haskell.error "getRight: Left"

ppkhToAddress :: PaymentPubKeyHash -> CardanoAddress 
ppkhToAddress = 
    getRight . Tx.toCardanoAddressInEra Nparams.testnet . plutusAddress 
    where    
    plutusAddress ppkh =
        Address (PubKeyCredential $ unPaymentPubKeyHash ppkh)
                (Nothing)
