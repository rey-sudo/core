#!/bin/bash
WORKDIR=$(pwd)
network=preview
config_url="https://book.world.dev.cardano.org"
node_path="$WORKDIR/bin/"
node_config_path="$WORKDIR/bin/configuration/$network"
node_bin_url="https://github.com/IntersectMBO/cardano-node/releases/download/8.7.1-pre/cardano-node-8.7.1-linux.tar.gz"


echo "Output: $node_path"

export PATH=$PATH:$node_path

echo "Output: $PATH"


if [ ! -d "$node_path" ]; then

    mkdir -p "$node_path"
    
    curl -L $node_bin_url | tar -xz -C ./bin --strip-components=1

    echo "Directory created: $node_path"
else
    echo "Directory already exists: $node_path"
fi



if [ ! -d "$node_config_path" ]; then

    mkdir -p "$node_config_path"

    curl -s $config_url/environments/$network/config.json > $node_config_path/config.json
    curl -s $config_url/environments/$network/db-sync-config.json  > $node_config_path/db-sync-config.json
    curl -s $config_url/environments/$network/submit-api-config.json  > $node_config_path/submit-api-config.json
    curl -s $config_url/environments/$network/topology.json  > $node_config_path/topology.json
    curl -s $config_url/environments/$network/byron-genesis.json  >   $node_config_path/byron-genesis.json
    curl -s $config_url/environments/$network/shelley-genesis.json  > $node_config_path/shelley-genesis.json
    curl -s $config_url/environments/$network/alonzo-genesis.json   > $node_config_path/alonzo-genesis.json
    curl -s $config_url/environments/$network/conway-genesis.json  > $node_config_path/conway-genesis.json

    echo "Directory created: $node_config_path"
else
    echo "Directory already exists: $node_config_path"
fi




wait

cabal exec plutus-example-deploy -- --verbose \
    --config pab/pab-config.yml \
    migrate  

cabal exec plutus-example-deploy -- --verbose \
    --config pab/pab-config.yml \
    --log-config pab/log-config.yaml \
    --passphrase pab123456789 \
    webserver

