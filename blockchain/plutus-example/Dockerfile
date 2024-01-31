FROM nixos/nix:latest as base-stage

VOLUME /app/pab

WORKDIR /app

COPY /bin /app/pab

COPY /tmp /nix/store

ENV NETWORK=preview
           
ENV NETWORK_ID=2 

ENV CONFIG="/app/pab/configuration/$NETWORK" 
 
ENV PATH="/app/pab:${PATH}" 

EXPOSE 9080

RUN plutus-example-deploy --verbose \
    --config $CONFIG/config.yaml \
    migrate  

CMD ["/bin/sh", "-c", "plutus-example-deploy --verbose \
    --config $CONFIG/config.yaml \
    --log-config $CONFIG/log.yaml \
    --passphrase pab123456789 \
    webserver"]
