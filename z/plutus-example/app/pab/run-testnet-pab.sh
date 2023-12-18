#!bin/bash


cabal exec plutus-example-deploy -- --verbose \
    --config app/pab/testnet/pab-config.yml \
    migrate  


#root path
cabal exec plutus-example-deploy -- --verbose \
    --config app/pab/testnet/pab-config.yml \
    --log-config app/pab/loggin.yaml \
    --ekg \
    webserver
 
#    --memory \


