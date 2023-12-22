#!/bin/bash


git clone https://github.com/IntersectMBO/cardano-node.git 

cd cardano-node

git fetch --all --recurse-submodules --tags

git checkout $(curl -s https://api.github.com/repos/IntersectMBO/cardano-node/releases/latest | jq -r .tag_name)