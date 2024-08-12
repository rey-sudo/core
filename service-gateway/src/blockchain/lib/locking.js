import {
  ColdWallet,
  Core,
  Data,
  Blaze,
  makeValue,
  Blockfrost,
} from "@blaze-cardano/sdk";
import { applyParamsToScript, cborToScript } from "@blaze-cardano/uplc";

const provider = new Blockfrost({
  network: "cardano-preprod",
  projectId: "preprodex26NYImZOT84XAA67qhyHyA7TT6PCGI",
});

const externalWallet = Core.addressFromBech32(
  "addr_test1qppygdhzm0t7nnlclmds3dy0wc3du870dpy48juu0xxuu2aefdfvc4e0785y7vfhwlmsn3rn26mzvv9md0mhnkpjlc4s0jshh4"
);

const stateMachineAddress = Core.addressFromBech32(
  "addr_test1wq9vdh0vpr0ngvn3cmrh2we2n56s70rukeakmmekmm7vk3c666ya4"
);

const wallet = new ColdWallet(externalWallet, 2, provider);

const blaze = await Blaze.from(provider, wallet);

///////////////////////////////////

const threadTokenUnit =
  "a0fb1dba6071ecc4c3e2c6781e5a5f000b156ceb98389a537120eea5746872656164746f6b656e";

const assetId = Core.AssetId(threadTokenUnit);

const threadTokenPolicyId = Core.AssetId.getPolicyId(assetId);

const assetName = Buffer.from(
  Core.AssetId.getAssetName(assetId),
  "hex"
).toString();

const threadTokenUtxos = await provider.getUnspentOutputsWithAsset(
  stateMachineAddress,
  assetId
);

if (threadTokenUtxos.length < 1) {
  throw new Error("ZERO_THREADTOKEN_UTXOS");
}

if (threadTokenUtxos.length > 1) {
  throw new Error("THREADTOKEN_QUANTITY");
}

for (const utxo of threadTokenUtxos) {
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

const stateMachineScript = cborToScript(
  applyParamsToScript(
    "5903cb0100003232323232323223223232232322533300a3232533300c3005300d375400226464646464646464646464a66602e602660306ea80244c8c8c8c8c8c8c94ccc078c068c07cdd500089919192999810980e98111baa001132323232533302500e1533302500b1533302500215333025003100114a029405280a503375e66e952004330283009330284c01010100330283752022660506ea003ccc0a0dd400725eb812f5c0600a604c6ea800ccdc4807192999812180e98129baa0011480004dd6981498131baa001325333024301d302537540022980103d87a80001323300100137566054604e6ea8008894ccc0a4004530103d87a8000132323232533302a33722911000021533302a3371e9101000021300f3302e375000297ae014c0103d87a8000133006006003375a60560066eb8c0a4008c0b4008c0ac004c8cc004004dd5980d18131baa00322533302800114c103d87a800013232323253330293372291100002153330293371e9101000021300e3302d374c00297ae014c0103d87a8000133006006003375660540066eb8c0a0008c0b0008c0a8004cc024c8cc004004dd5980c98129baa00222533302700114bd7009981418129814800998010011815000810181318119baa00116330043758600260446ea8c020c088dd500b919baf300930233754002601260466ea8c05cc08cdd50019181298131813000981198101baa00116330013758600a603e6ea8c014c07cdd500a119baf30063020375400202444646600200200644a666046002298103d87a8000132325333022300500213007330260024bd70099802002000981380118128009ba548000cc004dd6180f98101810181018101810181018101810180e1baa3002301c375402200e44646600200200644a66604000229404c94ccc078cdc79bae302300200414a226600600600260460024603c002602600c264a666030602260326ea8028528899251375a603a60346ea8024dd6980e180c9baa008375a603660380046eb4c068004c068008dd7180c000980c0011bad301600130123754002602800460266028002660220146602200e97ae03011300e37540022940c004c034dd50011180818088008a4c26cac64a666012600a0022a66601860166ea800c526161533300930020011533300c300b37540062930b0a99980499b874801000454ccc030c02cdd50018a4c2c2c60126ea8008dc3a4004a66600a6002600c6ea80084c8c8c8c8c8c8c8c94ccc040c04c00852616375a602200260220046eb4c03c004c03c008dd7180680098068011bad300b001300737540042c6e1d2000375c002ae6955ceaab9e5573eae815d0aba21",
    [threadTokenPolicyId],
    { dataType: "list", items: [{ dataType: "bytes" }] }
  ),
  "PlutusV2"
);

const productPrice = 50n * 1_000_000n;

const productCollateral = 25n * 1_000_000n;

const threadTokenAsset = makeValue(productPrice, ...[[threadTokenUnit, 1n]]);

const minFee = 3n * 1_000_000n;

const data = {
  state: 1n,
  seller: "424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b",
  collateral: productCollateral,
  price: productPrice
};

const Datum = Data.Object({
  state: Data.Integer(),
  seller: Data.Bytes(),
  collateral: Data.Integer(),
  price: Data.Integer()
});

const lockingDatum = Data.to(data, Datum);

const tx = await blaze
  .newTransaction()
  .addInput(threadTokenUtxos[0], stateMachineRedeemer)
  .lockAssets(stateMachineAddress, threadTokenAsset, lockingDatum)
  .provideScript(stateMachineScript)
  .addRequiredSigner("424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b")
  .setChangeAddress(externalWallet)
  .setMinimumFee(minFee)
  .complete();

const cbor = tx.toCbor();

console.log(cbor);
