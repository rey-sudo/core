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


bWalletBS :: B.ByteString
bWalletBS = "3f2ec097f77e4254df012d5d4d4b45e48459c6ec5795e92df30f2dbc"

sWalletBS :: B.ByteString
sWalletBS = "484ebc54b4112e54e1f7524dbdc6bb42635648a06c297e584592e80b"


buyerWallet :: Ledger.PaymentPubKeyHash
buyerWallet = Ledger.PaymentPubKeyHash (Ledger.PubKeyHash $ decodeHex bWalletBS)

sellerWallet :: Ledger.PaymentPubKeyHash
sellerWallet = Ledger.PaymentPubKeyHash (Ledger.PubKeyHash $ decodeHex sWalletBS)


sellerWallet' :: Wallet
sellerWallet' = knownWallet 1

defaultParams :: S.Params
defaultParams = S.Params { S.bWallet'     = buyerWallet
                         , S.pPrice'      = Ada.lovelaceOf 10_000_000
                         , S.sCollateral' = Ada.lovelaceOf 5_000_000
                         }

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    --setLocaleEncoding utf8
    Simulator.logString @(Builtin MarketplaceContracts) "Starting plutus-starter PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug
    Simulator.logString @(Builtin MarketplaceContracts) "********* PAB Server is running *********"
    void $ liftIO getLine
    cidInit <- Simulator.activateContract sellerWallet' SlaveContract
    Simulator.logString @(Builtin MarketplaceContracts) $ "wallet = " ++ show sellerWallet'
    
    let sp  = S.StartParams { S.startParams = defaultParams }
   
    void $ Simulator.callEndpointOnInstance cidInit "start" sp
    Simulator.waitNSlots 2



























handlers :: SimulatorEffectHandlers (Builtin MarketplaceContracts)
handlers =
    Simulator.mkSimulatorHandlers def
    $ interpret (contractHandler Builtin.handleBuiltin)


