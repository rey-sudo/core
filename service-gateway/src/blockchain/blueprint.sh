aiken build && npx @blaze-cardano/blueprint@latest plutus.json -o plutus.ts
cp -f plutus.json ./test
cp -f plutus.ts ./test