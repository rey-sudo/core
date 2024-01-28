#!/bin/bash

echo "extra-substituters  = https://cache.iog.io" >> /etc/nix/nix.conf 
echo "extra-experimental-features  = nix-command flakes" >> /etc/nix/nix.conf 
echo "trusted-users  = root" >> /etc/nix/nix.conf 
echo "extra-trusted-public-key = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" >> /etc/nix/nix.conf 

cd plutus-apps 

nix develop
