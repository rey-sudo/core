FROM alpine:latest as base-stage

WORKDIR /app

COPY /bin /app/index

RUN chmod +x /app/index

EXPOSE 30

CMD ["/bin/sh", "-c", "plutus-chain-index --socket-path $SOCKET_PATH/db/node.socket --db-path $CONFIG/index.db --port 9083 --network-id $NETWORK_ID start-index"]
