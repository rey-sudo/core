#!bin/bash


cabal exec plutus-example-deploy -- --verbose \
    --config app/pab/testnet/pab-config.yml \
    migrate  


#root path



cabal exec plutus-example-deploy -- --verbose \
    --config app/pab/pab-config.yml \
    --log-config app/pab/loggin.yaml \
    webserver
 





#    --memory \

#     --ekg \

#    --rollback-history 1000 \