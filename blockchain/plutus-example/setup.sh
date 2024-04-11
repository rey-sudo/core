#!/bin/bash

WORKDIR=$(pwd)
NETWORK=preview
bin_path="$WORKDIR/bin/"
pab_binary="$bin_path/plutus-example-deploy"
config_path="$WORKDIR/bin/configuration/$NETWORK"

if [ ! -x "$pab_binary" ]; then
    echo "Error: pab binary not found in /bin."
    exit 1
else
    echo "plutus-example-deploy binary found."
fi

export PATH=$PATH:$bin_path

echo "PATH =>: $PATH"


ldd_output=$(ldd $pab_binary)

paths=$(echo "$ldd_output" | awk -F '/' '/=> \/nix\/store\// {print $4}')

destination="$(pwd)/tmp"

for folder in $paths; do
    source_path="/nix/store/$folder"
    sudo cp -r "$source_path" "$destination"
    echo "Copied: $folder"
done

plutus-example-deploy --verbose \
    --config $config_path/config.yaml \
    migrate  


plutus-example-deploy --verbose \
    --config $config_path/config.yaml \
    --log-config $config_path/log.yaml \
    --passphrase pab123456789 \
    webserver


cabal exec plutus-example-deploy -- --verbose \
    --config bin/configuration/local/config.yaml \
    migrate
      


cabal exec plutus-example-deploy -- --verbose \
    --config bin/configuration/local/config.yaml \
    --log-config bin/configuration/local/log.yaml \
    --passphrase pab123456789 \
    webserver