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
    Input(..),
    SlaveError(..),
    SlaveSchema,
    Params(..),
    StartParams(..),
    LockingParams(..),
    DeliveredParams(..),
    ReceivedParams(..),
    contract,
    typedValidator
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

import Ledger.Tx.Constraints (TxConstraints, mustPayToAddress, mustBeSignedBy)
import Ledger.Tx.Constraints.OffChain qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger                     (Slot (..))  
import Ledger.Tx.CardanoAPI

import Plutus.Contract (ContractError, type (.\/), Endpoint, Contract, Promise, AsContractError(..), endpoint, logInfo, logWarn, ownFirstPaymentPubKeyHash, utxosAt, selectList)
import Plutus.Contract.Request as Wallet (getUnspentOutput)

import StateMachine (AsSMContractError (..), OnChainState, State (..), Void)
import StateMachine qualified as SM

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
    , pPrice      :: Ada.Ada
    , sCollateral :: Ada.Ada
    } | Appeal | Finished
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''SlaveState
PlutusTx.makeLift ''SlaveState

data Params = Params
    { sWallet'     :: PaymentPubKeyHash
    , bWallet'     :: PaymentPubKeyHash
    , pPrice'      :: Ada.Ada
    , sCollateral' :: Ada.Ada
    }
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Params
PlutusTx.makeLift ''Params

data Input = Locking | Delivered | Received
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Input
PlutusTx.makeLift ''Input

-- | Arguments for the @"Start"@ endpoint
data StartParams =
    StartParams
        {  sWalletParam       :: Haskell.String
        ,  bWalletParam       :: Haskell.String
        ,  pPriceParam        :: Integer
        ,  sCollateralParam   :: Integer
        } deriving stock (Haskell.Show, Generic)
          deriving anyclass (ToJSON, FromJSON)

-- | Arguments for the @"locking"@ endpoint
data LockingParams =
    LockingParams
        { lockingParams :: Params
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
        .\/ Endpoint "locking" LockingParams
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
transition :: Params -> State SlaveState -> Input -> Maybe (TxConstraints Void Void, State SlaveState)
transition params State{ stateData = oldData, stateValue = oldStateValue } input = case (oldData, input) of
    (SlaveState{cState, bWallet, pPrice}, Locking)                          | cState == 0           -> let constraints = mustBeSignedBy bWallet
                                                                                                           newValue   =  oldStateValue + (Ada.toValue pPrice)
                                                                                                       in Just (constraints,
                                                                                                          State{stateData = oldData { cState = 1
                                                                                                                                    , sLabel = "locking"
                                                                                                                                    , bSlot  = True
                                                                                                                                    }
                                                                                                                                    , stateValue = newValue })


    (SlaveState{cState, sWallet}, Delivered)                                | cState == 1           -> let constraints = mustBeSignedBy sWallet
                                                                                                       in Just (constraints,
                                                                                                          State{ stateData = oldData { cState = 2
                                                                                                                                     , sLabel = "delivered"
                                                                                                                                     , pDelivered = True
                                                                                                                                     }
                                                                                                                                     , stateValue = oldStateValue })


    (SlaveState{cState, bWallet, sWallet, sCollateral, pPrice}, Received)   | cState == 2           -> let money = Ada.toValue (sCollateral + pPrice)
                                                                                                           constraints = mustBeSignedBy bWallet
                                                                                                            <> mustPayToAddress (pubKeyHashAddress sWallet Nothing) money
                                                                                                       in Just (constraints,
                                                                                                          State{ stateData = Finished, stateValue = mempty })


    _                                                                                               -> Nothing

{-# INLINABLE machine #-}
machine :: Params -> SM.StateMachine SlaveState Input
machine params = SM.mkStateMachine Nothing (transition params) isFinal where
    isFinal Finished = True
    isFinal _        = False

{-# INLINABLE mkValidator #-}
mkValidator :: Params -> V2.ValidatorType (SM.StateMachine SlaveState Input)
mkValidator params = SM.mkValidator $ machine params

typedValidator :: Params -> V2.TypedValidator (SM.StateMachine SlaveState Input)
typedValidator = V2.mkTypedValidatorParam @(SM.StateMachine SlaveState Input)
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator @ScriptContextV2 @SlaveState @Input

-- //////////////////////////////////////////////////////////////////////////

client :: Params -> SM.StateMachineClient SlaveState Input
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
        endpoints      = selectList [startEndpoint, locking, delivered, received]

        locking        = endpoint @"locking" $ \LockingParams{ lockingParams } -> do
                                                    void (SM.runStep (client lockingParams) Locking)
                                                    logInfo @Text "Buyer:locking"

        delivered      = endpoint @"delivered" $ \DeliveredParams{ deliveredParams } -> do
                                                    void (SM.runStep (client deliveredParams) Delivered)
                                                    logInfo @Text "Seller:delivered"

        received       = endpoint @"received" $ \ReceivedParams{ receivedParams } -> do
                                                    void (SM.runStep (client receivedParams) Received)
                                                    logInfo @Text "Buyer:received"


startEndpoint :: Promise () SlaveSchema SlaveError ()
startEndpoint = endpoint @"Start" $ \StartParams{sWalletParam, bWalletParam, pPriceParam, sCollateralParam} -> do                     
              logInfo @Haskell.String "1"

              utxos <- utxosAt $ ppkhToAddress (bStoPPKH sWalletParam)

              logInfo @Haskell.String "2"

              let params = Params { sWallet'     = bStoPPKH sWalletParam 
                                  , bWallet'     = bStoPPKH bWalletParam
                                  , pPrice'      = Ada.lovelaceOf pPriceParam
                                  , sCollateral' = Ada.lovelaceOf sCollateralParam
                                  }

                  theClient       = client params 
                  theCollateral   = Ada.toValue (sCollateral' params)
                  theConstraints  = Haskell.mempty
                  theLookups      = (Constraints.unspentOutputs utxos)
                  theInitialState = initialState params
                  
              logInfo @Haskell.String "3"
              logInfo @Text "START_ENDPOINT"
              
              SM.runInitialiseWithUnbalanced theLookups theConstraints theClient theInitialState theCollateral
              

              





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

--(Just (StakingHash (PubKeyCredential $ unStakePubKeyHash $ spkh  w)))

bStoPPKH :: Haskell.String -> PaymentPubKeyHash
bStoPPKH bs = PaymentPubKeyHash $ pkhToPubKeyHash bs

bStoSPKH :: Haskell.String -> StakePubKeyHash
bStoSPKH bs = StakePubKeyHash (PubKeyHash $ decodeHex (B.pack bs))






              








--slotCfg :: SlotConfig
--slotCfg = def

--timer :: V2.POSIXTime
--timer = TimeSlot.slotToEndPOSIXTime slotCfg (Slot 50)

--deadlineRange :: VI.ValidityInterval V2.POSIXTime
--deadlineRange = VI.from timer

--      logInfo @String $ "runInitialiseWithUnbalanced " <> show utx
--      adjustUnbalancedTx utx >>= yieldUnbalancedTx
--      pure initialState


