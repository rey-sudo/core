FROM nixos/nix:latest as base-stage

WORKDIR /app

COPY /bin /app/index

COPY /tmp /nix/store

EXPOSE 90

CMD ["/bin/sh", "-c", "plutus-chain-index --socket-path $SOCKET_PATH/db/node.socket --db-path $CONFIG/index.db --port 9083 --network-id $NETWORK_ID start-index"]
