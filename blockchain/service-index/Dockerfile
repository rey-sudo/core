FROM nixos/nix:latest as base-stage

WORKDIR /app

COPY /bin /app/index

COPY /tmp /nix/store

ENV NETWORK=preview
           
ENV NETWORK_ID=2 

ENV CONFIG="/app/index/configuration/$NETWORK" 
 
ENV PATH="/app/index:${PATH}" 

ENV SOCKET_PATH="/app/node/configuration/$NETWORK/db/node.socket"          

EXPOSE 90

CMD ["/bin/sh", "-c", "plutus-chain-index --socket-path $SOCKET_PATH --db-path $CONFIG/index.db --port 9083 --network-id $NETWORK_ID start-index"]
