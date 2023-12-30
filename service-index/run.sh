#!/bin/bash

WORKDIR=$(pwd)
network=preview
network_id=2
index_path="$WORKDIR/bin/"


export PATH=$PATH:$index_path

echo "INDEX_PATH: $PATH"


cd ..

NODEDIR=$(pwd)

node_config_path="$NODEDIR/service-node/bin/configuration/$network"

export PATH=$PATH:$node_config_path

echo "NODE_PATH: $PATH"

cd service-index

#cabal exec -- plutus-chain-index \
#    --config testnet/chain-index-config.json \ 
#   start-index



plutus-chain-index --socket-path $node_config_path/db/node.socket \
    --db-path ./index.db \
    --config ./config.json \
    --network-id $network_id start-index



    