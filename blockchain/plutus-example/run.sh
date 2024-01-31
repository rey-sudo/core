#!/bin/bash
WORKDIR=$(pwd)
network=preview
bin_path="$WORKDIR/bin/"
config_path="$WORKDIR/bin/configuration/$network"

export PATH=$PATH:$bin_path


plutus-example-deploy --verbose \
    --config $config_path/config.yaml \
    migrate  


plutus-example-deploy --verbose \
    --config $config_path/config.yaml \
    --log-config $config_path/log.yaml \
    --passphrase pab123456789 \
    webserver
