{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Contracts(
    MarketplaceContracts(..)
    ) where


import           Data.Aeson                             (FromJSON (..), ToJSON (..))                                                      
import qualified Data.OpenApi                           as OpenApi
import           GHC.Generics                           (Generic)
import           Prettyprinter                          (Pretty (..), viaShow)
import           Slave                                  as S
import qualified Plutus.PAB.Effects.Contract.Builtin    as Builtin
import           Prelude                                hiding (init)
import           IntegrationTest                        as IT

data MarketplaceContracts = SlaveContract
                          | MasterContract
                          | IntegrationTest
                          deriving (Eq, Ord, Show, Generic)
                          deriving anyclass OpenApi.ToSchema
                          deriving anyclass (FromJSON, ToJSON)


instance Pretty MarketplaceContracts where
    pretty = viaShow
 

instance Builtin.HasDefinitions MarketplaceContracts where
    getDefinitions = [ SlaveContract, MasterContract, IntegrationTest ]
   
    getContract = \case
        SlaveContract    -> Builtin.SomeBuiltin S.contract
        IntegrationTest    -> Builtin.SomeBuiltin IT.run
     
