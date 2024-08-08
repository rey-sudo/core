import { ColdWallet, Core, Blaze, Blockfrost } from "@blaze-cardano/sdk";

let externalWallet = Core.addressFromBech32(
  "addr_test1qppygdhzm0t7nnlclmds3dy0wc3du870dpy48juu0xxuu2aefdfvc4e0785y7vfhwlmsn3rn26mzvv9md0mhnkpjlc4s0jshh4"
);

const targetWallet = Core.addressFromBech32(
  "addr_test1qp539v654clv34y7k6zwrtxzczwvzz0dudmgfy5rt3qvjf2hrg74pzy4umh8udkhshpqmwdzluk6zvr5tcrj8h74re2q2yavu8"
);

let contractAddress = Core.addressFromBech32(
  "addr_test1wp4ep7h3mw4fvse8v8lmafzjpettgfm972r783mzlcemzrg5avvkf"
);

const provider = new Blockfrost({
  network: "cardano-preview",
  projectId: "previewgTjbjYtdKdOcNmhtu6H9snNl3DhnaxQf",
});

const wallet = new ColdWallet(externalWallet, 2, provider);

const blaze = await Blaze.from(provider, wallet);

///////////////////////////////////

const btnUnit =
  "54a29c2626156de3af97cdead84264aaf0805857cc5c026af077fc3b746872656164746f6b656e";
const assetId = Core.AssetId(btnUnit);

const assetName = Buffer.from(
  Core.AssetId.getAssetName(assetId),
  "hex"
).toString();

const utxos = await provider.getUnspentOutputsWithAsset(contractAddress, assetId);

console.log(`UTxOs with ${assetName} Asset (FT):${utxos}`);

for (const utxo of utxos) {
    const utxoRef = `${utxo.input().transactionId()}#${utxo.input().index()}`;
    console.log(utxoRef);
  
    const amountADA = utxo.output().amount().coin();
    console.log(`Amount of ADA: ${amountADA / 1000000n}`);
  
    const amountBTN = utxo.output().amount().multiasset().get(btnUnit);
    console.log(`Amount of ${assetName}: ${amountBTN}`);
  }
  
  
//////////////////////////////////

const tx = await blaze
  .newTransaction()
  .payLovelace(targetWallet, 13n * 1_000_000n)
  .addRequiredSigner("424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b")
  .setChangeAddress(externalWallet)
  .complete();

const cbor = tx.toCbor();

console.log(cbor);







//"previewgTjbjYtdKdOcNmhtu6H9snNl3DhnaxQf"
