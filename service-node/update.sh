#!/bin/bash

source="https://book.world.dev.cardano.org"
network="preview"
output="configuration/$network"


curl -O -J $source/environments/$network/config.json &&
jq . config.json | ruby -ryaml -rjson -e 'puts YAML.dump(JSON.parse(STDIN.read))' > $output/config.yaml &&
rm config.json

curl -O -J $source/environments/$network/db-sync-config.json &&
jq . db-sync-config.json | ruby -ryaml -rjson -e 'puts YAML.dump(JSON.parse(STDIN.read))' > $output/db-sync-config.yaml &&
rm db-sync-config.json

curl -O -J $source/environments/$network/submit-api-config.json &&
jq . submit-api-config.json | ruby -ryaml -rjson -e 'puts YAML.dump(JSON.parse(STDIN.read))' > $output/submit-api-config.json.yaml &&
rm submit-api-config.json

curl -O -J $source/environments/$network/topology.json &&
jq . topology.json | ruby -ryaml -rjson -e 'puts YAML.dump(JSON.parse(STDIN.read))' > $output/topology.yaml &&
rm topology.json

curl -O -J $source/environments/$network/byron-genesis.json &&
jq . byron-genesis.json | ruby -ryaml -rjson -e 'puts YAML.dump(JSON.parse(STDIN.read))' > $output/byron-genesis.yaml &&
rm byron-genesis.json 

curl -O -J $source/environments/$network/shelley-genesis.json &&
jq . shelley-genesis.json | ruby -ryaml -rjson -e 'puts YAML.dump(JSON.parse(STDIN.read))' > $output/shelley-genesis.yaml &&
rm shelley-genesis.json

curl -O -J $source/environments/$network/alonzo-genesis.json &&
jq . alonzo-genesis.json | ruby -ryaml -rjson -e 'puts YAML.dump(JSON.parse(STDIN.read))' > $output/alonzo-genesis.yaml &&
rm alonzo-genesis.json

curl -O -J $source/environments/$network/conway-genesis.json &&
jq . conway-genesis.json | ruby -ryaml -rjson -e 'puts YAML.dump(JSON.parse(STDIN.read))' > $output/conway-genesis.yaml &&
rm conway-genesis.json