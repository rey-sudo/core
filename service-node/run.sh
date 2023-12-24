#!/bin/bash
WORKDIR=$(pwd)
network=preview
cardano_node_path="$WORKDIR/bin/"
node_config_path="$WORKDIR/bin/configuration/$network/"
source="https://book.world.dev.cardano.org"
node_binary_url="https://github.com/IntersectMBO/cardano-node/releases/download/8.7.1-pre/cardano-node-8.7.1-linux.tar.gz"


echo "Output: $cardano_node_path"

export PATH=$PATH:$cardano_node_path

echo "Output: $PATH"




if [ ! -d "$cardano_node_path" ]; then

    mkdir -p "$cardano_node_path"
    
    curl -L $node_binary_url | tar -xz -C ./bin --strip-components=1

    echo "Directory created: $cardano_node_path"
else
    echo "Directory already exists: $cardano_node_path"
fi



if [ ! -d "$node_config_path" ]; then

    mkdir -p "$node_config_path"

    curl -s $source/environments/$network/config.json > $node_config_path/config.json
    curl -s $source/environments/$network/db-sync-config.json  > $node_config_path/db-sync-config.json
    curl -s $source/environments/$network/submit-api-config.json  > $node_config_path/submit-api-config.json
    curl -s $source/environments/$network/topology.json  > $node_config_path/topology.json
    curl -s $source/environments/$network/byron-genesis.json  >   $node_config_path/byron-genesis.json
    curl -s $source/environments/$network/shelley-genesis.json  > $node_config_path/shelley-genesis.json
    curl -s $source/environments/$network/alonzo-genesis.json   > $node_config_path/alonzo-genesis.json
    curl -s $source/environments/$network/conway-genesis.json  > $node_config_path/conway-genesis.json

    echo "Directory created: $node_config_path"
else
    echo "Directory already exists: $node_config_path"
fi




wait



cardano-node run \
    --config $node_config_path/config.json \
    --database-path $node_config_path/db/ \
    --socket-path $node_config_path/db/node.socket \
    --host-addr 0.0.0.0 \
    --port 3001 \
    --topology $node_config_path/topology.json \

