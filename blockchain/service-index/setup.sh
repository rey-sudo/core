#!/bin/bash

WORKDIR=$(pwd)
NETWORK=preview
network_id=2
index_bin_path="$WORKDIR/bin/"
index_binary="$index_bin_path/plutus-chain-index"
config_path="$WORKDIR/bin/configuration/$NETWORK"


if [ ! -x "$index_binary" ]; then
    echo "Error: index binary not found in /bin."
    exit 1
else
    echo "plutus-chain-index binary found."
fi

ldd_output=$(ldd $index_binary)

paths=$(echo "$ldd_output" | awk -F '/' '/=> \/nix\/store\// {print $4}')

destination="$(pwd)/tmp"

for folder in $paths; do
    source_path="/nix/store/$folder"
    sudo cp -r "$source_path" "$destination"
    echo "Copied: $folder"
done


export PATH=$PATH:$index_bin_path

echo "PATH=>: $PATH"

cd ..

ROOTDIR=$(pwd)

node_config_path="$ROOTDIR/service-node/bin/configuration/$NETWORK"

export PATH=$PATH:$node_config_path

echo "NODE_PATH: $PATH"

cd $WORKDIR


plutus-chain-index --socket-path $node_config_path/db/node.socket \
    --db-path $index_bin_path/configuration/$NETWORK/index.db \
    --port 9083 \
    --network-id 2 \
     start-index



    