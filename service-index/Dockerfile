FROM alpine:latest as base-stage

WORKDIR /app

COPY /bin /app/index

ENV PATH="/app/index:${PATH}"

RUN chmod +x /app/index

EXPOSE 30

CMD ["./index/plutus-chain-index"]
