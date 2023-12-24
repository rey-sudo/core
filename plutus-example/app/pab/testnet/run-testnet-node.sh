#!/usr/bin/env bash

cardano-node run \
    --config testnet/config.json \
    --topology testnet/topology.json \
    --database-path testnet/db \
    --socket-path testnet/node.sock \
    --port 9082
