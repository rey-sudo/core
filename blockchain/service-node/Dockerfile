FROM alpine:latest as base-stage

WORKDIR /app

COPY /bin /app/node

RUN chmod +x /app/node

ENV NETWORK=preview
             
ENV PATH="/app/node:${PATH}"

ENV CONFIG="/app/node/configuration/$NETWORK"

EXPOSE 3001

CMD ["/bin/sh", "-c", "cardano-node run --config $CONFIG/config.json --database-path $CONFIG/db/ --socket-path $CONFIG/db/node.socket --host-addr 0.0.0.0 --port 3001 --topology $CONFIG/topology.json"]




