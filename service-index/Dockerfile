FROM nixos/nix:latest as base-stage

WORKDIR /app

RUN test -d /plutus-apps || (echo "Error: Local folder not found try repo.sh!" && exit 1)

COPY /plutus-apps /app/plutus-apps

COPY ./setup.sh /app

RUN chmod +x /app

EXPOSE 90

CMD ["/bin/sh", "-c", "sh ./setup.sh"]
