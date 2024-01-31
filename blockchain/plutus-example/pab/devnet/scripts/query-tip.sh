#!/bin/sh

ROOT=private-testnet

export CARDANO_NODE_SOCKET_PATH=$ROOT/node-bft1/node.sock

cardano-cli query tip --testnet-magic 42 | jq '.era'

cardano-cli query utxo --address $(cat private-testnet/addresses/user1.addr) --testnet-magic 42