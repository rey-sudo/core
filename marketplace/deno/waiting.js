import {
  ColdWallet,
  Core,
  Data,
  Blaze,
  makeValue,
  Blockfrost,
} from "@blaze-cardano/sdk";

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
  network: "cardano-preprod",
  projectId: "preprodex26NYImZOT84XAA67qhyHyA7TT6PCGI",
});

const wallet = new ColdWallet(externalWallet, 2, provider);

const blaze = await Blaze.from(provider, wallet);

///////////////////////////////////

const threadTokenUnit =
  "eae54750459b753aa3502c0afed7d62b8e7b3e9a070c8a0024aec934746872656164746f6b656e";

const assetId = Core.AssetId(threadTokenUnit);

const assetName = Buffer.from(
  Core.AssetId.getAssetName(assetId),
  "hex"
).toString();

const utxos = await provider.getUnspentOutputsWithAsset(
  contractAddress,
  assetId
);

console.log(`UTxOs with ${assetName} Asset (FT):${utxos}`);

for (const utxo of utxos) {
  const utxoRef = `${utxo.input().transactionId()}#${utxo.input().index()}`;
  console.log(utxoRef);

  const amountADA = utxo.output().amount().coin();
  console.log(`Amount of ADA: ${amountADA / 1000000n}`);

  const amountBTN = utxo.output().amount().multiasset().get(threadTokenUnit);
  console.log(`Amount of ${assetName}: ${amountBTN}`);
}

//////////////////////////////////

const stateMachineRedeemer = Core.PlutusData.newConstrPlutusData(
  new Core.ConstrPlutusData(0n, new Core.PlutusList())
);

const stateMachineScript = Core.Script.newPlutusV2Script(
  new Core.PlutusV2Script(
    Core.HexBlob(
      "590193010000323232323232322323223225333007323232323232533300d300b300e375400a264a66601c601800620022940c8cc004004dd61809980a180a180a180a180a180a180a180a18081baa30133010375401044a66602400229404c94ccc040cdc79bae301500200414a2266006006002602a002264a66601c66e1d2002300f375400c264944dd7180998081baa006132533300f3370e900218081baa00714a2264944dd6980a18089baa006375a602660206ea8014dd6980918079baa004375c602260240046eb4c040004c030dd50009807001180698070009980580319805801a5eb8052613656325333006300400115333009300837540042930b0a99980319b87480080044c8c94ccc02cc03800852616375c601800260106ea800854ccc018cdc3a40080022a66601260106ea800852616153330063370e90030008a99980498041baa00214985858c018dd5000a999801980098021baa002132323232533300a300d002149858dd7180580098058011bad3009001300537540042c6e1d20005734aae7555cf2ab9f5740ae855d101"
    )
  )
);

const minUtxoValue = 10n * 1_000_000n;

const productPrice = 25n * 1_000_000n;

const threadTokenAsset = makeValue(productPrice, ...[[threadTokenUnit, 1n]]);

const minFee = 3n * 1_000_000n;


const data = {
  state: 1n,
  seller: "424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b",
};

const Datum = Data.Object({
  state: Data.Integer(),
  seller: Data.Bytes(),
});

const waitingDatum = Data.to(data, Datum);

const tx = await blaze
  .newTransaction()
  .addInput(utxos[0], stateMachineRedeemer)
  .lockAssets(contractAddress, threadTokenAsset, waitingDatum)
  .provideScript(stateMachineScript)
  .addRequiredSigner("424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b")
  .setChangeAddress(externalWallet)
  .setMinimumFee(minFee)
  .complete();

const cbor = tx.toCbor();

console.log(cbor);
