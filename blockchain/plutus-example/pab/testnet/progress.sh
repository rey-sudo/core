#!bin/bash

export CARDANO_NODE_SOCKET_PATH=$(pwd)/node.sock

echo $CARDANO_NODE_SOCKET_PATH

cardano-cli get-tip --testnet-magic 2

