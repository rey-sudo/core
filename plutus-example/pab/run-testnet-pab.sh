#!bin/bash


cabal exec plutus-example-deploy -- --verbose \
    --config pab/pab-config.yml \
    migrate  


#root path

#export SHELLEY_TEST_DATA=/home/debian/plutus-apps/plutus-pab/local-cluster/cluster-data/cardano-node-shelley/


cabal exec plutus-example-deploy -- --verbose \
    --config pab/pab-config.yml \
    --log-config pab/log-config.yaml \
    --passphrase pab123456789 \
    webserver
 





#    --memory \

#     --ekg \

#    --rollback-history 1000 \