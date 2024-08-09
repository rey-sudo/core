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
  "addr_test1wpfntc3at9rffxyh59258c3z7wgnhkqjrqm49evcwp7cckgmkvjq0"
);

const wallet = new ColdWallet(externalWallet, 2, provider);

const blaze = await Blaze.from(provider, wallet);

///////////////////////////////////

const threadTokenUnit =
  "d686789520a6983db317cfd1e05d73066458c47bb5067e1e32288e3f746872656164746f6b656e";

const assetId = Core.AssetId(threadTokenUnit);

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
    "5902c70100003232323232323223223232232322533300a3232323232325333010300c3011375400a2646464a666026601e00a20042940c8ccc004004dd61801980a9baa3003301537540149000111299980c00108008999801801980d80119b80325333016300f301737540022900009bad301b3018375400264a66602c601e602e6ea80045300103d87a8000132330010013756603860326ea8008894ccc06c004530103d87a8000132323232533301c33722911000021533301c3371e91010000213374a9000198101ba80014bd700a6103d87a8000133006006003375a603a0066eb8c06c008c07c008c074004c8cc004004dd5991800980c9baa300130193754603800846038603a00244a666034002298103d87a8000132323232533301b33722911000021533301b3371e91010000213374a90001980f9ba60014bd700a6103d87a8000133006006003375660380066eb8c068008c078008c070004004c8cc004004dd6180b980c180c180c180c180c180c180c180c180a1baa30023014375401244a66602c00229404c94ccc050cdc79bae301900200514a226600600600260320024602c002264a666022601460246ea80184c9289bae30163013375400c264a66602466e1d20043013375400e29444c9289bad30173014375400c6eb4c058c04cdd50029bad3015301237540086eb8c050c054008dd6980980098079baa0013011002301030110013300e0073300e0044bd700a4c26cac64a666012600a0022a66601860166ea800c5261615333009300200113232533300e3011002149858dd7180780098059baa003153330093370e90020008a99980618059baa00314985854ccc024cdc3a400c0022a66601860166ea800c5261616300937540046e1d20025333005300130063754004264646464a666018601e0042930b1bae300d001300d002375a6016002600e6ea800858dc3a40006eb80055cd2ab9d5573caae7d5d02ba157441",
    ["d686789520a6983db317cfd1e05d73066458c47bb5067e1e32288e3f"],
    { dataType: "list", items: [{ dataType: "bytes" }] }
  ),
  "PlutusV2"
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
  .addInput(threadTokenUtxos[0], stateMachineRedeemer)
  .lockAssets(stateMachineAddress, threadTokenAsset, waitingDatum)
  .provideScript(stateMachineScript)
  .addRequiredSigner("424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b")
  .setChangeAddress(externalWallet)
  .setMinimumFee(minFee)
  .complete();

const cbor = tx.toCbor();

console.log(cbor);
