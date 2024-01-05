{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}

module Slave(
    SlaveState(..),
    SlaveInput(..),
    SlaveError(..),
    SlaveSchema,
    Params(..),
    StartParams(..),
    LockingParams(..),
    DeliveredParams(..),
    ReceivedParams(..),
    contract,
    typedValidator,
    ppkhToCardanoAddress,
    stringToPPKH
    ) where

import Control.Lens
import Control.Monad (forever, void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Monoid (Last (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger (PubKeyHash(..), toPlutusAddress, toPubKeyHash, pubKeyHashAddress, txOutAddress, pubKeyHashTxOut, stakingCredential)
import Ledger.Address (CardanoAddress(..), PaymentPrivateKey (PaymentPrivateKey, unPaymentPrivateKey),
                       PaymentPubKey (PaymentPubKey, unPaymentPubKey),
                       PaymentPubKeyHash (PaymentPubKeyHash, unPaymentPubKeyHash),
                       StakePubKey (StakePubKey, unStakePubKey), StakePubKeyHash (StakePubKeyHash, unStakePubKeyHash),
                       stakePubKeyHashCredential)

import Ledger.Tx.Constraints (TxConstraints, mustSpendAtLeast,mustPayToAddress, mustBeSignedBy)
import Ledger.Tx.Constraints.OffChain qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger                     (Slot (..))  
import Ledger.Tx.CardanoAPI

import Plutus.Contract (ContractError, type (.\/), Endpoint, Contract, Promise, AsContractError(..), endpoint, logInfo, logWarn, ownFirstPaymentPubKeyHash, utxosAt, selectList)
import Plutus.Contract.Request as Wallet (getUnspentOutput)

import StateMachine (AsSMContractError (..), OnChainState, State (..), Void)
import StateMachine qualified as SM

import Plutus.Script.Utils.Ada (Ada)
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Typed (ScriptContextV2)
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2
import Plutus.Script.Utils.Value (AssetClass, TokenName, Value)
import PlutusTx qualified
import PlutusTx.Prelude hiding (Applicative (..), check)
--------
import qualified Data.ByteString.Char8 as B

import Utilities

import Plutus.V2.Ledger.Api qualified as V2
import Plutus.V2.Ledger.Contexts qualified as V2
import Cardano.Node.Emulator.Internal.Node.TimeSlot as TimeSlot

import Data.Default               (def)

import Plutus.ChainIndex.Config
import Cardano.Node.Emulator.Internal.Node.Params qualified as Nparams
import Ledger.Tx.CardanoAPI.Internal qualified as Tx
import Cardano.Api qualified as C
import Cardano.Api.Byron qualified as C
import Cardano.Api.Shelley qualified as C
import Plutus.V1.Ledger.Api (Address (Address), Credential (PubKeyCredential), StakingCredential (StakingHash))
import Control.Monad.Error.Lens (throwing)
import Data.Either (fromRight, either)
import Data.Maybe (fromJust)
---
import Ledger.Typed.Scripts (ValidatorTypes (..))
import Ledger.Typed.Scripts qualified as Scripts
--------
import Prelude qualified as Haskell

data SlaveState = SlaveState
    { cState      :: Integer
    , sLabel      :: BuiltinByteString
    , bSlot       :: Bool
    , pDelivered  :: Bool
    , pReceived   :: Bool
    , sWallet     :: PaymentPubKeyHash
    , bWallet     :: Maybe PaymentPubKeyHash
    , pPrice      :: Ada
    , sCollateral :: Ada
    } | Appeal | Finished
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''SlaveState
PlutusTx.makeLift ''SlaveState

data Params = Params
    { sWallet'     :: Maybe PaymentPubKeyHash
    , bWallet'     :: Maybe PaymentPubKeyHash
    , pPrice'      :: Maybe Ada
    , sCollateral' :: Maybe Ada
    }
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Params
PlutusTx.makeLift ''Params

data SlaveInput = Locking | Delivered | Received
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''SlaveInput
PlutusTx.makeLift ''SlaveInput

-- | Arguments for the @"Start"@ endpoint
data StartParams =
    StartParams
        {  sWalletParam       :: Haskell.String
        ,  pPriceParam        :: Integer
        ,  sCollateralParam   :: Integer
        } deriving stock (Haskell.Show, Generic)
          deriving anyclass (ToJSON, FromJSON)

-- | Arguments for the @"locking"@ endpoint
data LockingParams =
    LockingParams
        { bWalletParam       :: Haskell.String
        } deriving stock (Haskell.Show, Generic)
          deriving anyclass (ToJSON, FromJSON)

-- | Arguments for the @"delivered"@ endpoint
data DeliveredParams =
    DeliveredParams
        { deliveredParams :: Params
        } deriving stock (Haskell.Show, Generic)
          deriving anyclass (ToJSON, FromJSON)

-- | Arguments for the @"received"@ endpoint
data ReceivedParams =
    ReceivedParams
        { receivedParams :: Params
        } deriving stock (Haskell.Show, Generic)
          deriving anyclass (ToJSON, FromJSON)

type SlaveSchema =
        Endpoint "Start" StartParams
        .\/ Endpoint "Locking" LockingParams
        .\/ Endpoint "delivered" DeliveredParams
        .\/ Endpoint "received" ReceivedParams

data SlaveError =
    SlaveContractError ContractError
    | SlaveSMError SM.SMContractError
    | StoppedUnexpectedly
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''SlaveError

instance AsSMContractError SlaveError where
    _SMContractError = _SlaveSMError

instance AsContractError SlaveError where
    _ContractError = _SlaveContractError

{-# INLINABLE transition #-}
transition :: Params -> State SlaveState -> SlaveInput -> Maybe (TxConstraints Void Void, State SlaveState)
transition params State{ stateData = oldData, stateValue = oldStateValue } input = case (oldData, input) of
    (SlaveState{cState, pPrice}, Locking)   |   cState == 0     -> let newValue    =  oldStateValue + (Ada.toValue pPrice)
                                                                       constraints =  case (bWallet' params) of 
                                                                            Just b -> mustBeSignedBy b
                                                                            Nothing -> mempty 
                                                                   in Just (constraints,
                                                                      State{stateData = oldData { cState = 1
                                                                                                , sLabel = "locking"
                                                                                                , bSlot  = True
                                                                                                }
                                                                                                , stateValue = newValue })


    (SlaveState{cState, sWallet}, Delivered)    | cState == 1   -> let constraints = mustBeSignedBy sWallet
                                                                   in Just (constraints,
                                                                      State{ stateData = oldData { cState = 2
                                                                                                 , sLabel = "delivered"
                                                                                                 , pDelivered = True
                                                                                                 }
                                                                                                 , stateValue = oldStateValue })


    (SlaveState cState
                sLabel
                bSlot
                pDelivered
                pReceived
                sWallet 
                (Just bWallet)
                pPrice
                sCollateral, Received)   | cState == 2   -> let money = Ada.toValue (sCollateral + pPrice)
                                                                constraints = mustBeSignedBy bWallet
                                                                    <> mustPayToAddress (pubKeyHashAddress sWallet Nothing) money
                                                            in Just (constraints,
                                                               State{ stateData = Finished, stateValue = mempty })


    _ -> Nothing


type SlaveStateMachine = SM.StateMachine SlaveState SlaveInput

{-# INLINABLE machine #-}
machine :: Params -> SlaveStateMachine
machine params = SM.mkStateMachine Nothing (transition params) isFinal where
    isFinal Finished = True
    isFinal _        = False


{-# INLINABLE mkValidator #-}
mkValidator :: Params -> V2.ValidatorType SlaveStateMachine
mkValidator params = SM.mkValidator $ machine params


typedValidator :: Params -> V2.TypedValidator SlaveStateMachine
typedValidator = V2.mkTypedValidatorParam @SlaveStateMachine
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator


client :: Params -> SM.StateMachineClient SlaveState SlaveInput
client params = SM.mkStateMachineClient $ SM.StateMachineInstance (machine params) (typedValidator params)


initialState :: Params -> SlaveState
initialState params = SlaveState { cState = 0
                                 , sLabel = "waiting"
                                 , bSlot  = False
                                 , pDelivered = False
                                 , pReceived = False
                                 , sWallet = fromJust $ sWallet' params
                                 , bWallet = bWallet' params
                                 , pPrice = fromJust $ pPrice' params
                                 , sCollateral = fromJust $ sCollateral' params
                                 }


contract :: Contract () SlaveSchema SlaveError ()
contract = forever endpoints where
        endpoints      = selectList [startEndpoint, lockingEndpoint, delivered, received]

        delivered      = endpoint @"delivered" $ \DeliveredParams{ deliveredParams } -> do
                                                    void (SM.runStep (client deliveredParams) Delivered)
                                                    logInfo @Text "Seller:delivered"

        received       = endpoint @"received" $ \ReceivedParams{ receivedParams } -> do
                                                    void (SM.runStep (client receivedParams) Received)
                                                    logInfo @Text "Buyer:received"


startEndpoint :: Promise () SlaveSchema SlaveError ()
startEndpoint = endpoint @"Start" $ \StartParams{sWalletParam, pPriceParam, sCollateralParam} -> do  
                  
              let params = Params { sWallet'     = stringToPPKH sWalletParam
                                  , bWallet'     = Nothing
                                  , pPrice'      = integerToAda pPriceParam
                                  , sCollateral' = integerToAda sCollateralParam
                                  }
               
                  theClient       = client params
                  theInitialState = initialState params
                  theCollateral   = Ada.toValue $ fromJust (sCollateral' params)
                  theConstraints  = mustBeSignedBy $ fromJust (sWallet' params)
                  theLookups      = Haskell.mempty

              utxos <- utxosAt $ contractAddress params

              logInfo @Haskell.String  $ "////UTXOSx///" <> Haskell.show utxos
                                   
              logInfo @Text "START_ENDPOINT"
              
              void (SM.runInitialiseWithUnbalanced theLookups theConstraints theClient theInitialState theCollateral)
              
             
lockingEndpoint :: Promise () SlaveSchema SlaveError ()
lockingEndpoint = endpoint @"Locking" $ \LockingParams{bWalletParam} -> do
   
                let params = Params { sWallet'     = Nothing
                                    , bWallet'     = stringToPPKH bWalletParam
                                    , pPrice'      = Nothing
                                    , sCollateral' = Nothing
                                    }

                let theClient       = client params
                    theConstraints  = Haskell.mempty
                    theLookups      = Haskell.mempty
                
                logInfo @Text "LOCKING_ENDPOINT"

                void (SM.runStepWithUnbalanced theLookups theConstraints theClient Locking)
                

stringToPPKH :: Haskell.String -> Maybe PaymentPubKeyHash
stringToPPKH bs = Just (PaymentPubKeyHash $ pkhToPubKeyHash bs)


integerToAda :: Integer -> Maybe Ada
integerToAda amount = Just (Ada.lovelaceOf amount)


contractAddress :: Params -> CardanoAddress
contractAddress = Scripts.validatorCardanoAddress Nparams.testnet . typedValidator














getRight :: Haskell.Either a b -> b
getRight (Haskell.Right x) = x
getRight (Haskell.Left _)  = Haskell.error "getRight: Left"

ppkhToCardanoAddress :: PaymentPubKeyHash -> CardanoAddress 
ppkhToCardanoAddress = 
    getRight . Tx.toCardanoAddressInEra Nparams.testnet . plutusAddress 
    where    
    plutusAddress ppkh =
        Address (PubKeyCredential $ unPaymentPubKeyHash ppkh)
                (Nothing)

--(Just (StakingHash (PubKeyCredential $ unStakePubKeyHash $ spkh  w)))







--bStoSPKH :: Haskell.String -> StakePubKeyHash
--bStoSPKH bs = StakePubKeyHash (PubKeyHash $ decodeHex (B.pack bs))


            






--slotCfg :: SlotConfig
--slotCfg = def

--timer :: V2.POSIXTime
--timer = TimeSlot.slotToEndPOSIXTime slotCfg (Slot 50)

--deadlineRange :: VI.ValidityInterval V2.POSIXTime
--deadlineRange = VI.from timer

--      logInfo @String $ "runInitialiseWithUnbalanced " <> show utx
--      adjustUnbalancedTx utx >>= yieldUnbalancedTx
--      pure initialState


