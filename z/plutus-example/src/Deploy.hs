{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}

module Deploy where

import qualified Plutus.V2.Ledger.Api as PlutusV2
import           PlutusTx             (BuiltinData, compile, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool, Eq ((==)), Integer, traceIfFalse,
                                       ($))
import           Prelude              (IO)
import           Utilities            (wrapValidator, writeValidatorToFile)
import           Slave                as S
import Plutus.Script.Utils.Ada qualified as Ada
import Wallet.Emulator.Wallet as Wallet
import Plutus.Contract.Test
import           Ledger.CardanoWallet   qualified as CW
import           Ledger.Address                      (Address, PaymentPubKeyHash, pubKeyHashAddress)


defaultWalletPaymentPubKeyHash :: PaymentPubKeyHash
defaultWalletPaymentPubKeyHash = CW.paymentPubKeyHash (CW.fromWalletNumber $ CW.WalletNumber 1)



params :: S.Params
params = S.Params { S.bWallet'     = defaultWalletPaymentPubKeyHash
                  , S.pPrice'      = Ada.lovelaceOf 10_000_000
                  , S.sCollateral' = Ada.lovelaceOf 5_000_000
                  }





serializedScript :: IO ()
serializedScript = writeValidatorToFile "./output/slave.plutus" (S.typedValidator params)
