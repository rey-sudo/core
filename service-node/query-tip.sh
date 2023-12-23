#!/bin/bash
WORKDIR=$(pwd)
network=$1
cardano_node_path="$WORKDIR/bin/"
node_config_path="$WORKDIR/bin/configuration/$network/"
source="https://book.world.dev.cardano.org"

echo "Output: $cardano_node_path"
export PATH=$PATH:$cardano_node_path
echo "Output: $PATH"

cardano-cli query tip --testnet-magic 2 --socket-path $node_config_path/db/node.socket