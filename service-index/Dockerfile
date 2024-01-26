FROM alpine:latest as base-stage

WORKDIR /app

COPY /bin /app/bin

ENV PATH="/app/bin:${PATH}"

RUN chmod +x /app/bin/

EXPOSE 30

CMD ["/bin/sh", "-c", "plutus-chain-index"]
