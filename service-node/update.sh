#!/bin/bash

source="https://book.world.dev.cardano.org"
network="preview"
output="configuration/$network"


curl -s $source/environments/$network/config.json | yq -Poy - > $output/config.yaml 

curl -s $source/environments/$network/db-sync-config.json | yq -Poy -  > $output/db-sync-config.yaml

curl -s $source/environments/$network/submit-api-config.json | yq -Poy - > $output/submit-api-config.json.yaml 

curl -s $source/environments/$network/topology.json | yq -Poy -  > $output/topology.yaml 

curl -s $source/environments/$network/byron-genesis.json | yq -Poy -  > $output/byron-genesis.yaml 

curl -s $source/environments/$network/shelley-genesis.json | yq -Poy -  > $output/shelley-genesis.yaml 

curl -s $source/environments/$network/alonzo-genesis.json | yq -Poy -  > $output/alonzo-genesis.yaml 

curl -s $source/environments/$network/conway-genesis.json | yq -Poy - > $output/conway-genesis.yaml
