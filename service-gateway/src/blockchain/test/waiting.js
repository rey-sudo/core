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
  "addr_test1wqu29yd0r3pzdh6pas9rgnhxn9dcyt6rkm3xyqs4cz8kylc0u2lpe"
);

const wallet = new ColdWallet(externalWallet, 2, provider);

const blaze = await Blaze.from(provider, wallet);

///////////////////////////////////

const threadTokenUnit =
  "d9ec13627b55c95a3ceb513f17dd3288d1ee62e17fa2f098ee52ed07746872656164746f6b656e";

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
    "5903f50100003232323232323223223232232322533300a3232533300c3005300d37540022646464646464646464a66602a6022602c6ea801c4c8c8c8c8c8c8c94ccc070c060c074dd50008991919299980f980d98101baa001132323232533302300e1533302300b1533302300215333023003100114a029405280a503375e66e952004330263009330264c0101010033026375201e6604c6ea00392f5c097ae030053024375400666e24034c94ccc088c06cc08cdd50008a400026eb4c09cc090dd5000992999811180d98119baa00114c0103d87a80001323300100137566050604a6ea8008894ccc09c004530103d87a800013232323253330283372291100002153330283371e9101000021300f3302c375000297ae014c0103d87a8000133006006003375a60520066eb8c09c008c0ac008c0a4004c8cc004004dd5980c18121baa00322533302600114c103d87a800013232323253330273372291100002153330273371e9101000021300e3302b374c00297ae014c0103d87a8000133006006003375660500066eb8c098008c0a8008c0a0004cc024c8cc004004dd5980b98119baa00222533302500114bd700998131811981380099801001181400080f181218109baa00116330043758600260406ea8c020c080dd500a919baf300930213754002601260426ea8c054c084dd500191811981218120009810980f1baa00116330013758600a603a6ea8c014c074dd5009119baf3006301e375400202044646600200200644a666042002298103d87a8000132325333020300500213007330240024bd70099802002000981280118118009ba548000cc004dd6180e980f180f180f180f180f180f180f180f180d1baa3002301a375401e00a44646600200200644a66603c00229404c94ccc070cdc79bae302100200414a22660060060026042002460380026022008264a66602c601e602e6ea80204c9289bae301b30183754010264a66602e66e1d20043018375401229444c9289bad301c301937540106eb4c06cc060dd50039bad301a3017375400c6eb4c064c068008dd7180c000980c0011bad301600130123754002602800460266028002660220146602200e97ae03011300e37540022940c004c034dd50011180818088008a4c26cac64a666012600a0022a66601860166ea800c5261615333009300200113232533300e3011002149858dd7180780098059baa003153330093370e90020008a99980618059baa00314985854ccc024cdc3a400c0022a66601860166ea800c5261616300937540046e1d200253330053001300637540042646464646464a66601c60220042930b1bad300f001300f002375c601a002601a0046eb4c02c004c01cdd50010b1b8748000dd7000ab9a5573aaae7955cfaba05742ae881",
    [threadTokenPolicyId],
    { dataType: "list", items: [{ dataType: "bytes" }] }
  ),
  "PlutusV2"
);

const minUtxoValue = 10n * 1_000_000n;

const productPrice = 25n * 1_000_000n;

const productCollateral = 25n * 1_000_000n;

const threadTokenAsset = makeValue(productPrice, ...[[threadTokenUnit, 1n]]);

const minFee = 3n * 1_000_000n;

const data = {
  state: 1n,
  seller: "424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b",
  collateral: productCollateral
};

const Datum = Data.Object({
  state: Data.Integer(),
  seller: Data.Bytes(),
  collateral: Data.Integer(),
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
