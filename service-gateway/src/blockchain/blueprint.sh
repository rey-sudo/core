aiken build && npx @blaze-cardano/blueprint@latest plutus.json -o plutus.ts
cp -f plutus.json ./lib
cp -f plutus.ts ./lib