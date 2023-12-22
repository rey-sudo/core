#!bin/bash


cabal exec plutus-example-deploy -- --verbose \
    --config app/pab/pab-config.yml \
    migrate  


#root path

#export SHELLEY_TEST_DATA=/home/debian/plutus-apps/plutus-pab/local-cluster/cluster-data/cardano-node-shelley/


cabal exec plutus-example-deploy -- --verbose \
    --config app/pab/pab-config.yml \
    --log-config app/pab/log-config.yaml \
    webserver
 





#    --memory \

#     --ekg \

#    --rollback-history 1000 \