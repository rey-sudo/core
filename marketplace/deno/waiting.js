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
  "addr_test1wp5fgzvgjpctu6s3kw8jk0qqx622phr7znsf0943r45lwssjzf58q"
);

const wallet = new ColdWallet(externalWallet, 2, provider);

const blaze = await Blaze.from(provider, wallet);

///////////////////////////////////

const threadTokenUnit =
  "f2592b7a63989d549077b1ff614f41371f00d7e03ccb7b2cee9ac1f4746872656164746f6b656e";

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
    "5902da0100003232323232323223223232232322533300a32533300b3004300c37546002601a6ea80084c8c8c8c8c8c94ccc044c034c048dd50028991919299980a180800288010a503233300100137586006602c6ea8c00cc058dd5005a4000444a66603200420022666006006603800466e00c94ccc05cc040c060dd50008a400026eb4c070c064dd500099299980b9808180c1baa00114c0103d87a8000132330010013756603a60346ea8008894ccc070004530103d87a8000132323232533301d33722911000021533301d3371e91010000213374a9000198109ba80014bd700a6103d87a8000133006006003375a603c0066eb8c070008c080008c078004c8cc004004dd59806980c9baa300d30193754603800644a666036002298103d87a8000132323232533301c33722911000021533301c3371e91010000213374a9000198101ba60014bd700a6103d87a80001330060060033756603a0066eb8c06c008c07c008c074004004c8cc004004dd6180c180c980c980c980c980c980c980c980c980a9baa30023015375401444a66602e00229404c94ccc054cdc79bae301a00200514a226600600600260340024602e002264a666024601660266ea80184c9289bae30173014375400c264a66602666e1d20043014375400e29444c9289bad30183015375400c6eb4c05cc050dd50029bad3016301337540086eb8c054c058008dd6980a00098081baa0013012002301130120013300f0083300f0054bd700a5023010301100114984d958c94ccc024c01400454ccc030c02cdd50018a4c2c2a666012600400226464a66601c60220042930b1bae300f001300b37540062a66601266e1d20040011533300c300b37540062930b0a99980499b874801800454ccc030c02cdd50018a4c2c2c60126ea8008dc3a4004a66600a6002600c6ea80084c8c8c8c94ccc030c03c00852616375c601a002601a0046eb4c02c004c01cdd50010b1b8748000dd7000ab9a5573aaae7955cfaba05742ae881",
    [threadTokenPolicyId],
    { dataType: "list", items: [{ dataType: "bytes" }] }
  ),
  "PlutusV2"
);

const minUtxoValue = 10n * 1_000_000n;

const productPrice = 11n * 1_000_000n;

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
  .addInput(threadTokenUtxos[0], stateMachineRedeemer)
  .lockAssets(stateMachineAddress, threadTokenAsset, waitingDatum)
  .provideScript(stateMachineScript)
  .addRequiredSigner("424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b")
  .setChangeAddress(externalWallet)
  .setMinimumFee(minFee)
  .complete();

const cbor = tx.toCbor();

console.log(cbor);
