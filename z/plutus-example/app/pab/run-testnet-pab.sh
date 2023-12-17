#!bin/bash


cabal exec plutus-example-deploy -- --config app/pab/testnet/pab-config.yml migrate
cabal exec plutus-example-deploy -- --config app/pab/testnet/pab-config.yml webserver

