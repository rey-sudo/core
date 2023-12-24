#!/bin/bash
WORKDIR=$(pwd)
network=preview
config_url="https://book.world.dev.cardano.org"
wallet_path="$WORKDIR/bin/"
wallet_bin_url="https://github.com/cardano-foundation/cardano-wallet/releases/download/v2022-07-01/cardano-wallet-v2022-07-01-linux64.tar.gz"

export PATH=$PATH:$wallet_path

echo "WALLET_PATH: $PATH"

cd ..

NODEDIR=$(pwd)

node_config_path="$NODEDIR/service-node/bin/configuration/$network"

export PATH=$PATH:$node_config_path

echo "NODE_PATH: $PATH"

cd service-wallet 

if [ ! -d "$wallet_path" ]; then

    mkdir -p "$wallet_path"
    
    curl -L $wallet_bin_url | tar -xz -C ./bin --strip-components=1

    echo "Directory created: $wallet_path"
else
    echo "Directory already exists: $wallet_path"
fi

cardano-wallet serve \
      --testnet $node_config_path/byron-genesis.json \
      --node-socket $node_config_path/db/node.socket \
      --database ./wallet-db

      