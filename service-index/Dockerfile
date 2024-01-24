FROM alpine:latest as base-stage

WORKDIR /app

COPY /bin /app/data

RUN chmod +x /app/data

EXPOSE 30

CMD ["/bin/sh", "-c", "plutus-chain-index --socket-path $CONFIG/db/node.socket --db-path $CONFIG/index.db --port 9083 --network-id $NETWORK_ID start-index"]
