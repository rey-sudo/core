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
    DefaultEndpointParams(..),
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
    , bWallet     :: PaymentPubKeyHash
    , pPrice      :: Ada
    , sCollateral :: Ada
    } | Appeal | Finished
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''SlaveState
PlutusTx.makeLift ''SlaveState

data Params = Params
    { sWallet'     :: PaymentPubKeyHash
    , bWallet'     :: PaymentPubKeyHash
    , pPrice'      :: Ada
    , sCollateral' :: Ada
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


data DefaultEndpointParams = 
    DefaultEndpointParams
        {  sWalletParam       :: Haskell.String
        ,  bWalletParam       :: Haskell.String
        ,  pPriceParam        :: Integer
        ,  sCollateralParam   :: Integer
        } deriving stock (Haskell.Show, Generic)
          deriving anyclass (ToJSON, FromJSON)


-- | @Start endpoint
data StartParams =
    StartParams { startDefault :: DefaultEndpointParams
                } deriving stock (Haskell.Show, Generic)
                  deriving anyclass (ToJSON, FromJSON)


-- | @Locking endpoint
data LockingParams =
    LockingParams { lockingDefault :: DefaultEndpointParams
                  } deriving stock (Haskell.Show, Generic)
                    deriving anyclass (ToJSON, FromJSON)


-- | @delivered endpoint
data DeliveredParams =
    DeliveredParams { deliveredDefault :: DefaultEndpointParams
                    } deriving stock (Haskell.Show, Generic)
                      deriving anyclass (ToJSON, FromJSON)


-- | @received endpoint
data ReceivedParams =
    ReceivedParams { receivedDefault :: DefaultEndpointParams
                   } deriving stock (Haskell.Show, Generic)
                     deriving anyclass (ToJSON, FromJSON)


type SlaveSchema =
        Endpoint "Start" StartParams
        .\/ Endpoint "Locking" LockingParams
        .\/ Endpoint "Delivered" DeliveredParams
        .\/ Endpoint "Received" ReceivedParams

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
    (SlaveState{cState, bWallet, pPrice}, Locking)   |   cState == 0    -> let newValue    =  oldStateValue + (Ada.toValue pPrice)
                                                                               constraints =  mustBeSignedBy bWallet
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


    (SlaveState {cState, sWallet, bWallet, pPrice, sCollateral}, Received)  |   cState == 2     -> let money = Ada.toValue (sCollateral + pPrice)
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
                                 , sWallet = sWallet' params
                                 , bWallet = bWallet' params
                                 , pPrice = pPrice' params
                                 , sCollateral = sCollateral' params
                                 }


contract :: Contract () SlaveSchema SlaveError ()
contract = forever endpoints where
        endpoints      = selectList [startEndpoint, lockingEndpoint, deliveredEndpoint, receivedEndpoint]


generateParams :: DefaultEndpointParams -> Params
generateParams params = Params { sWallet'     = stringToPPKH $ sWalletParam params
                               , bWallet'     = stringToPPKH $ bWalletParam params
                               , pPrice'      = integerToAda $ pPriceParam params
                               , sCollateral' = integerToAda $ sCollateralParam params
                               }


startEndpoint :: Promise () SlaveSchema SlaveError ()
startEndpoint = endpoint @"Start" $ \StartParams{startDefault} -> do  
                  
              let params          = generateParams startDefault               
                  theClient       = client params
                  theInitialState = initialState params
                  theCollateral   = Ada.toValue $ (sCollateral' params)
                  theConstraints  = mustBeSignedBy $ (sWallet' params)
                  theLookups      = Haskell.mempty

              logInfo @Text "START_ENDPOINT"
              
              void (SM.runInitialiseWithUnbalanced theLookups theConstraints theClient theInitialState theCollateral)
              
             
lockingEndpoint :: Promise () SlaveSchema SlaveError ()
lockingEndpoint = endpoint @"Locking" $ \LockingParams{lockingDefault} -> do
   
                let params          = generateParams lockingDefault
                    theClient       = client params
                    theSeller       = sWallet' params
                    theConstraints  = Haskell.mempty
                    theLookups      = Haskell.mempty
                
                logInfo @Text "LOCKING_ENDPOINT"

                void (SM.runStepWithUnbalanced theLookups theConstraints theClient Locking theSeller)


deliveredEndpoint :: Promise () SlaveSchema SlaveError ()
deliveredEndpoint = endpoint @"Delivered" $ \DeliveredParams{deliveredDefault} -> do
                                                    let params    = generateParams deliveredDefault
                                                        theClient = client params

                                                    logInfo @Text "DELIVERED_ENDPOINT"  

                                                    void (SM.runStep theClient Delivered)


receivedEndpoint :: Promise () SlaveSchema SlaveError ()
receivedEndpoint = endpoint @"Received" $ \ReceivedParams{receivedDefault} -> do
                                                    let params    = generateParams receivedDefault
                                                        theClient = client params

                                                    logInfo @Text "RECEIVED_ENDPOINT"  

                                                    void (SM.runStep theClient Received)


stringToPPKH :: Haskell.String -> PaymentPubKeyHash
stringToPPKH bs = PaymentPubKeyHash $ pkhToPubKeyHash bs


integerToAda :: Integer -> Ada
integerToAda amount = Ada.lovelaceOf amount


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




--   utxos <- utxosAt $ contractAddress params

--    logInfo @Haskell.String  $ "////UTXOSx///" <> Haskell.show utxos
                                   


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


