FROM inputoutput/cardano-node as node-stage

WORKDIR /app

VOLUME /app/data

COPY . /app/data

RUN ls -al /app/data/configuration

EXPOSE 30

RUN cardano-node run \
      --config /app/data/configuration/preview/config.yaml \
      --database-path /app/data/configuration/preview/db/ \
      --socket-path /app/data/configuration/preview/db/node.socket \
      --host-addr 127.0.0.1 \
      --port 3001 \
      --topology /app/data/configuration/preview/topology.yaml



