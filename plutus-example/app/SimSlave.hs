{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}


module Main
    ( main
    ) where
        

import           Control.Monad                       (void)
import           Control.Monad.Freer                 (interpret)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (Result (..), fromJSON)
import           Data.Default                        (def)
import qualified Data.Monoid                         as Monoid
import           Ledger.Address                      (Address, PaymentPubKeyHash, pubKeyHashAddress)
import           Ledger.CardanoWallet   qualified as CW
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler(contractHandler))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Wallet.Emulator.Wallet              (Wallet, knownWallet)
import           Contracts                           (MarketplaceContracts(..))
import           Slave                               as S
import           Plutus.Script.Utils.Ada qualified as Ada
import           Utilities            (wrapValidator, writeTypedValidator, writeDataToFile, decodeHex)
import qualified Data.ByteString.Char8 as B
import qualified Ledger  
import Utilities

sellerWallet :: Wallet
sellerWallet = knownWallet 1

sellerWalletPPKH :: PaymentPubKeyHash
sellerWalletPPKH = CW.paymentPubKeyHash (CW.fromWalletNumber $ CW.WalletNumber 1)

buyerWallet :: Wallet
buyerWallet = knownWallet 2

buyerWalletPPKH :: Maybe Ledger.PaymentPubKeyHash
buyerWalletPPKH = Just (CW.paymentPubKeyHash (CW.fromWalletNumber $ CW.WalletNumber 2))


defaultParams :: S.DefaultEndpointParams
defaultParams = S.DefaultEndpointParams { S.sWalletParam      = sWalletBS
                                        , S.pPriceParam       = 10000000
                                        , S.sCollateralParam  = 5000000
                                        }





sWalletBS :: String
sWalletBS = "484ebc54b4112e54e1f7524dbdc6bb42635648a06c297e584592e80b"


bWalletBS :: String
bWalletBS = "3f2ec097f77e4254df012d5d4d4b45e48459c6ec5795e92df30f2dbc"

stringToPPKH :: String -> PaymentPubKeyHash
stringToPPKH bs = Ledger.PaymentPubKeyHash $ pkhToPubKeyHash bs

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    --setLocaleEncoding utf8
    Simulator.logString @(Builtin MarketplaceContracts) "Starting plutus-starter PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug
    Simulator.logString @(Builtin MarketplaceContracts) "********* PAB Server is running *********"
    void $ liftIO getLine

    sellerCID <- Simulator.activateContract sellerWallet SlaveContract
    Simulator.logString @(Builtin MarketplaceContracts) $ "wallet = " ++ show sellerWallet ++ " CID: " ++ show sellerCID

    let sp  = S.StartParams{ S.startDefault = defaultParams
                           }
    
    void $ Simulator.callEndpointOnInstance sellerCID "Start" sp
    Simulator.logString @(Builtin MarketplaceContracts) $ "Params = " ++ show sp
    Simulator.waitNSlots 2
    
    buyerCID <- Simulator.activateContract buyerWallet SlaveContract
    Simulator.logString @(Builtin MarketplaceContracts) $ "wallet = " ++ show buyerWallet ++ " CID: " ++ show buyerCID
    Simulator.waitNSlots 2
    
    void $ liftIO getLine

    let lockingInput  = S.LockingParams { S.lockingDefault = defaultParams
                                        , S.bWalletParam      = bWalletBS
                                        }


    void $ Simulator.callEndpointOnInstance buyerCID "Locking" lockingInput
    Simulator.waitNSlots 2

    Simulator.logString @(Builtin MarketplaceContracts) "//////////////////////////////////////"

    void $ liftIO getLine

    let deliveredInput  = S.DeliveredParams { S.deliveredDefault = defaultParams }
    void $ Simulator.callEndpointOnInstance sellerCID "Delivered" deliveredInput
    Simulator.waitNSlots 2

    shutdown


    



























handlers :: SimulatorEffectHandlers (Builtin MarketplaceContracts)
handlers =
    Simulator.mkSimulatorHandlers def
    $ interpret (contractHandler Builtin.handleBuiltin)


