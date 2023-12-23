{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}

module Spec.Slave(tests) where

import Control.Monad (void)
import Data.Maybe (isNothing)
import Plutus.Contract
import Plutus.Contract.StateMachine (OnChainState)
import Plutus.Contract.Test
import Slave (Input, Params, SlaveError, SlaveSchema, SlaveState, contract)
import Slave qualified as Slave
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Trace.Emulator qualified as Trace
import Test.Tasty
import Wallet.Emulator.Wallet as Wallet

sWalletBS :: String
sWalletBS = "484ebc54b4112e54e1f7524dbdc6bb42635648a06c297e584592e80b"


bWalletBS :: String
bWalletBS = "3f2ec097f77e4254df012d5d4d4b45e48459c6ec5795e92df30f2dbc"


params :: Slave.Params
params = Slave.Params { Slave.sWallet'     = mockWalletPaymentPubKeyHash w1
                      , Slave.bWallet'     = mockWalletPaymentPubKeyHash w2
                      , Slave.pPrice'      = Ada.lovelaceOf 10_000_000
                      , Slave.sCollateral' = Ada.lovelaceOf 5_000_000
                      }

theContract :: Contract () SlaveSchema SlaveError ()
theContract = do
            void $ Slave.contract


startParams :: Slave.StartParams
startParams = Slave.StartParams{ Slave.sWalletParam     = sWalletBS
                               , Slave.bWalletParam     = bWalletBS
                               , Slave.pPriceParam      = 10_000_000
                               , Slave.sCollateralParam = 5_000_000
                               }

lockingParams :: Slave.LockingParams
lockingParams = Slave.LockingParams { Slave.lockingParams = params }

deliveredParams :: Slave.DeliveredParams
deliveredParams = Slave.DeliveredParams { Slave.deliveredParams = params }

receivedParams :: Slave.ReceivedParams
receivedParams = Slave.ReceivedParams { Slave.receivedParams = params }


newSlot :: Trace.EmulatorTrace ()
newSlot = do
    seller <- Trace.activateContractWallet w1 theContract
    buyer <- Trace.activateContractWallet w2 theContract
    _ <- Trace.waitNSlots 5
    Trace.callEndpoint @"start" seller startParams
    void $ Trace.waitNSlots 5
    Trace.callEndpoint @"locking" buyer lockingParams
    void $ Trace.waitNSlots 5
    Trace.callEndpoint @"delivered" seller deliveredParams
    void $ Trace.waitNSlots 5
    Trace.callEndpoint @"received" buyer receivedParams
    void $ Trace.waitNSlots 5

tests :: TestTree
tests = testGroup "SLAVE"
    [ checkPredicate "activate endpoints"
        (endpointAvailable @"start" theContract (Trace.walletInstanceTag w1))
        newSlot
    ]

--collateral must be ada
--only can be locking by buller
