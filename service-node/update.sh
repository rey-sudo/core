#!/bin/bash

source="https://book.world.dev.cardano.org"
network="preview"
output="configuration/$network"

curl -O -J $source/environments/$network/config.json &&
jq . config.json | ruby -ryaml -rjson -e 'puts YAML.dump(JSON.parse(STDIN.read))' > $output/config.yaml

curl -O -J $source/environments/$network/db-sync-config.json
jq . db-sync-config.json | ruby -ryaml -rjson -e 'puts YAML.dump(JSON.parse(STDIN.read))' > $output/db-sync-config.yaml

curl -O -J $source/environments/$network/submit-api-config.json
jq . submit-api-config.json | ruby -ryaml -rjson -e 'puts YAML.dump(JSON.parse(STDIN.read))' > $output/submit-api-config.json.yaml

curl -O -J $source/environments/$network/topology.json
jq . topology.json | ruby -ryaml -rjson -e 'puts YAML.dump(JSON.parse(STDIN.read))' > $output/topology.yaml

curl -O -J $source/environments/$network/byron-genesis.json
jq . byron-genesis.json | ruby -ryaml -rjson -e 'puts YAML.dump(JSON.parse(STDIN.read))' > $output/byron-genesis.yaml

curl -O -J $source/environments/$network/shelley-genesis.json
jq . shelley-genesis.json | ruby -ryaml -rjson -e 'puts YAML.dump(JSON.parse(STDIN.read))' > $output/shelley-genesis.yaml

curl -O -J $source/environments/$network/alonzo-genesis.json
jq . alonzo-genesis.json | ruby -ryaml -rjson -e 'puts YAML.dump(JSON.parse(STDIN.read))' > $output/alonzo-genesis.yaml

curl -O -J $source/environments/$network/conway-genesis.json
jq . conway-genesis.json | ruby -ryaml -rjson -e 'puts YAML.dump(JSON.parse(STDIN.read))' > $output/conway-genesis.yaml