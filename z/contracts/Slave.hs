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
-- | A state machine with two states and two roles that take turns.
module Plutus.Contracts.Slave(
    SlaveState(..),
    Input(..),
    SlaveError(..),
    SlaveSchema,
    Params(..),
    contract
    ) where

import Control.Lens
import Control.Monad (forever, void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Monoid (Last (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger (PaymentPubKeyHash)
import Ledger.Tx.Constraints (TxConstraints)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract
import Plutus.Contract.StateMachine (AsSMContractError (..), OnChainState, State (..), Void)
import Plutus.Contract.StateMachine qualified as SM
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Typed (ScriptContextV2)
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2
import Plutus.Script.Utils.Value (AssetClass, TokenName, Value)
import PlutusTx qualified
import PlutusTx.Prelude hiding (Applicative (..), check)

import Prelude qualified as Haskell

data SlaveState = SlaveState
    { cState     :: Integer
    , sLabel     :: BuiltinByteString
    , pDelivered :: Bool
    , pReceived  :: Bool
    , sWallet    :: PaymentPubKeyHash
    , opToken    :: SM.ThreadToken
    , pIndex     :: Integer
    } | Finished
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Params = Params
    { pIndex' :: Integer }


data Input = StartSession | Locking | Delivered | Finish
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

type SlaveSchema =
        Endpoint "start" ()
        .\/ Endpoint "locking" ()
        .\/ Endpoint "delivered" ()
        .\/ Endpoint "received" ()

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
transition params State{ stateData = oldData, stateValue } input = case (oldData, input) of
    (SlaveState{sWallet, opToken, cState, pIndex}, Locking) | cState == 0                               -> Just (mempty,
                                                                                                           State{ stateData = SlaveState { sWallet, opToken, cState = 1, pIndex }, stateValue })


    (SlaveState{sWallet, opToken, cState, pIndex}, Delivered) | cState == 1                             -> Just (mempty,
                                                                                                           State{ stateData = SlaveState { sWallet, opToken, cState = 2, pIndex }, stateValue })


    (_,      Finish)                                                                                    -> Just (mempty,
                                                                                                           State{ stateData = Finished, stateValue = mempty })


    _                                                                                                   -> Nothing

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

client :: Params -> SM.StateMachineClient SlaveState Input
client params = SM.mkStateMachineClient $ SM.StateMachineInstance (machine params) (typedValidator params)

contract :: Params -> Contract () SlaveSchema SlaveError ()
contract params = forever endpoints where
        theClient = client params
        endpoints = selectList [start, locking, delivered, finish]
        start =  endpoint @"start" $ \() -> do
                                            pkh <- ownFirstPaymentPubKeyHash
                                            tt  <- SM.getThreadToken
                                            let initialState = SlaveState
                                                            { cState = 0
                                                            , sLabel = "waiting"
                                                            , pDelivered = False
                                                            , pReceived = False
                                                            , sWallet = pkh
                                                            , opToken = tt
                                                            , pIndex = 0
                                                            }
                                            void $ SM.runInitialise theClient initialState (Ada.lovelaceValueOf 1)
                                            logInfo @Text "INITIAL STATE"

        locking = endpoint @"locking" $ \() -> void (SM.runStep theClient Locking)

        delivered = endpoint @"delivered" $ \() -> void (SM.runStep theClient Delivered)

        finish = endpoint @"received" $ \() -> void (SM.runStep theClient Finish)



















PlutusTx.unstableMakeIsData ''SlaveState
PlutusTx.makeLift ''SlaveState

PlutusTx.unstableMakeIsData ''Params
PlutusTx.makeLift ''Params

PlutusTx.unstableMakeIsData ''Input
PlutusTx.makeLift ''Input

