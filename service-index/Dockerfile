FROM debian:latest as base-stage

WORKDIR /app

COPY /bin /app/bin

ENV PATH="/app/bin:${PATH}"

RUN chmod +x /app/bin

#RUN apt-get update -y && apt-get upgrade -y

#RUN apt-get install automake build-essential pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ tmux git jq wget libncursesw5 libtool autoconf -y

EXPOSE 30

CMD ["/bin/sh", "-c", "plutus-chain-index --socket-path $SOCKET_PATH/db/node.socket --db-path $CONFIG/index.db --port 9083 --network-id $NETWORK_ID start-index"]
