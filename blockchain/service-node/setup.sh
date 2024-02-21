#!/bin/bash
WORKDIR=$(pwd)
NETWORK=preview
config_url="https://book.world.dev.cardano.org"
node_filename="cardano-node-8.7.1-linux.tar.gz"
node_bin_url="https://github.com/IntersectMBO/cardano-node/releases/download/8.7.1-pre/$node_filename"
node_path="$WORKDIR/bin/"
node_config_path="$WORKDIR/bin/configuration/$NETWORK"


export PATH=$PATH:$node_path

echo "PATH =>: $PATH"


if [ ! -f "$node_filename" ]; then
    echo "$node_filename not found. Downloading..."

    wget $node_bin_url
else
    echo "$node_filename already exists."
fi


if [ ! -d "$node_path" ]; then

    mkdir -p "$node_path"
    
    tar -zxvf "$node_filename" -C "$node_path"

    echo "Directory created: $node_path"
else
    echo "Directory already exists: $node_path"
fi


if [ ! -d "$node_config_path" ]; then

    mkdir -p "$node_config_path"

    curl -s $config_url/environments/$NETWORK/config.json > $node_config_path/config.json
    curl -s $config_url/environments/$NETWORK/db-sync-config.json  > $node_config_path/db-sync-config.json
    curl -s $config_url/environments/$NETWORK/submit-api-config.json  > $node_config_path/submit-api-config.json
    curl -s $config_url/environments/$NETWORK/topology.json  > $node_config_path/topology.json
    curl -s $config_url/environments/$NETWORK/byron-genesis.json  >   $node_config_path/byron-genesis.json
    curl -s $config_url/environments/$NETWORK/shelley-genesis.json  > $node_config_path/shelley-genesis.json
    curl -s $config_url/environments/$NETWORK/alonzo-genesis.json   > $node_config_path/alonzo-genesis.json
    curl -s $config_url/environments/$NETWORK/conway-genesis.json  > $node_config_path/conway-genesis.json

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
    --topology $node_config_path/topology.json 

