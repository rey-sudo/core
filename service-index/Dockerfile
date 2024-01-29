FROM nixos/nix:latest as base-stage

WORKDIR /app

VOLUME /nix/store

COPY /bin /app/index

COPY /tmp /nix/store

ENV PATH="/app/index:${PATH}"

EXPOSE 90

CMD ["/bin/sh", "-c", "plutus-chain-index"]
