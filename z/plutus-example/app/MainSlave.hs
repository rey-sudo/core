module Main (main) where

import Cardano.Api
import Options.Applicative
import Slave
import Deploy


main :: IO ()
main = do
     _ <- writeSlavePlutus
     writeDatumInit
     return ()