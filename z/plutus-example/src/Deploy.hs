{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}

module Deploy where

import qualified Plutus.V2.Ledger.Api as PlutusV2
import           PlutusTx                           (Data (..))
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import qualified PlutusTx.Prelude as PlutusPrelude 
import           Utilities            (wrapValidator, writeTypedValidator, writeDataToFile)
import           Slave                as S
import Plutus.Script.Utils.Ada qualified as Ada
import Wallet.Emulator.Wallet as Wallet
import Plutus.Contract.Test
import           Ledger.CardanoWallet   qualified as CW
import           Ledger.Address                      (Address, PaymentPubKeyHash, pubKeyHashAddress)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Ledger      
import qualified PlutusTx.Prelude as PlutusPrelude 


bWalletBS :: B.ByteString
bWalletBS = "3f2ec097f77e4254df012d5d4d4b45e48459c6ec5795e92df30f2dbc"

sWalletBS :: B.ByteString
sWalletBS = "484ebc54b4112e54e1f7524dbdc6bb42635648a06c297e584592e80b"


buyerWallet :: Ledger.PaymentPubKeyHash
buyerWallet = Ledger.PaymentPubKeyHash (Ledger.PubKeyHash $ decodeHex bWalletBS)

sellerWallet :: Ledger.PaymentPubKeyHash
sellerWallet = Ledger.PaymentPubKeyHash (Ledger.PubKeyHash $ decodeHex sWalletBS)


params :: S.Params
params = S.Params { S.bWallet'     = buyerWallet
                  , S.pPrice'      = Ada.lovelaceOf 10_000_000
                  , S.sCollateral' = Ada.lovelaceOf 5_000_000
                  }



writeSlavePlutus :: IO ()
writeSlavePlutus = writeTypedValidator "./output/slave.plutus" (S.typedValidator params)


writeDatumInit :: IO ()
writeDatumInit = 
    let initialState = S.SlaveState { cState = 0
                              , sLabel = "waiting"
                              , bSlot  = False
                              , pDelivered = False
                              , pReceived = False
                              , sWallet = sellerWallet
                              , bWallet = bWallet' params
                              , pPrice = pPrice' params
                              , sCollateral = sCollateral' params
                           --   , mToken = tt
                              }
    in 
        writeDataToFile "./output/slave.datum" initialState





-- | Decode from hex base 16 to a base 10 bytestring is needed because
--   that is how it is stored in the ledger onchain
decodeHex :: B.ByteString -> PlutusPrelude.BuiltinByteString
decodeHex hexBS =    
         case getTx of
            Right decHex -> do
                PlutusPrelude.toBuiltin(decHex)  
            Left _ -> do
                PlutusPrelude.emptyByteString 
                
        where        
            getTx :: Either String B.ByteString = B16.decode hexBS
