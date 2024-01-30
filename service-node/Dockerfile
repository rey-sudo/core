FROM alpine:latest as base-stage

WORKDIR /node

COPY /bin /node

RUN chmod +x /node

EXPOSE 30

CMD ["/bin/sh", "-c", "cardano-node run --config $CONFIG/config.json --database-path $CONFIG/db/ --socket-path $CONFIG/db/node.socket --host-addr 0.0.0.0 --port 3001 --topology $CONFIG/topology.json"]




