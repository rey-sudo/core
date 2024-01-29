FROM nixos/nix:latest as base-stage

WORKDIR /app

RUN chmod +x /app

COPY /plutus-apps /app/plutus-apps

RUN echo "extra-substituters  = https://cache.iog.io" >> /etc/nix/nix.conf \
    && echo "extra-experimental-features  = nix-command flakes" >> /etc/nix/nix.conf  \
    && echo "trusted-users  = root" >> /etc/nix/nix.conf \
    && echo "extra-trusted-public-key = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" >> /etc/nix/nix.conf 

EXPOSE 90

RUN cd plutus-apps && nix develop

CMD ["/bin/sh", "-c", "cd plutus-apps && nix develop"]
