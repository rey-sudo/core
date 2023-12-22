#!/bin/bash

WORKDIR = "/app"

cardano-node run \
      --config $WORKDIR/cardano/testnet/config.json \
      --database-path $WORKDIR/cardano/testnet/db/ \
      --socket-path $WORKDIR/cardano/testnet/db/node.socket \
      --host-addr 127.0.0.1 \
      --port 1337 \
      --topology $WORKDIR/cardano/testnet/topology.json
