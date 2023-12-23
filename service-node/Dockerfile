FROM inputoutput/cardano-node as node-stage

WORKDIR /app

VOLUME /app/data

COPY . /app/data

RUN ls -al /app/data/configuration

EXPOSE 30

RUN cardano-node run \
      --config /data/configuration/preview/config.json \
      --database-path /data/configuration/preview/db/ \
      --socket-path /data/configuration/preview/db/node.socket \
      --host-addr 127.0.0.1 \
      --port 3001 \
      --topology /data/configuration/preview/topology.json



