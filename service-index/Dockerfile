FROM debian:latest as base-stage

WORKDIR /app

COPY /bin /app/index

ENV PATH="/app/index:${PATH}"

RUN chmod +x /app/index

RUN apt-get update -y && apt-get upgrade -y

RUN apt-get install automake build-essential pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ tmux git jq wget libncursesw5 libtool autoconf -y

EXPOSE 30

CMD ["/bin/sh", "-c", "cd index && ls && ldd ./plutus-chain-index"]
