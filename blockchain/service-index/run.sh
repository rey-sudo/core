#!/bin/bash

WORKDIR=$(pwd)
network=preview
network_id=2
index_path="$WORKDIR/bin/"
config_path="$WORKDIR/bin/configuration"

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
    --port 9083 \
    --network-id 2 \
     start-index



    