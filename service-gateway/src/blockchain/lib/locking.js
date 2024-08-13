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
  "addr_test1wquta3y3dgel3rw8lyzcxaegsgc0v3meq20kgxskerh9n7gt64q3g"
);

const wallet = new ColdWallet(externalWallet, 2, provider);

const blaze = await Blaze.from(provider, wallet);

///////////////////////////////////

const threadTokenUnit =
  "a1652f13f2a27f5dcaafa875543d00f63629d8b36d11bf7d9f50b8f6746872656164746f6b656e";

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



const stateMachineInput = Data.Enum([
  Data.Object({ Locking: Data.Object({ buyer: Data.Bytes() }) }),
  Data.Literal("Delivered"),
  Data.Literal("Received"),
]);

const datumUp = { Locking: { buyer: "424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b" } };

const stateMachineRedeemer = Data.to(datumUp, stateMachineInput);


const stateMachineScript = cborToScript(
  applyParamsToScript(
    "5904300100003232323232323223223232322322533300a3232533300c3007300d375400226464646464646464646464a66602e602660306ea80244c8c8c8c8c8c8c8c94ccc07cc06cc080dd500089919192999811180f18119baa001132323232533302600e1533302600b1533302600215333026003100114a029405280a503375e66e952004330293009330294c01010100330293752024660526ea0040cc0a4dd4007998149804998149ba900e4bd7025eb812f5c0600a604e6ea800ccdc4807192999812981018131baa0011480004dd6981518139baa0013253330253020302637540022980103d87a8000132330010013756605660506ea8008894ccc0a8004530103d87a8000132323232533302b33722911000021533302b3371e9101000021300f3302f375000297ae014c0103d87a8000133006006003375a60580066eb8c0a8008c0b8008c0b0004c8cc004004dd5980d98139baa00322533302900114c103d87a8000132323232533302a33722911000021533302a3371e9101000021300e3302e374c00297ae014c0103d87a8000133006006003375660560066eb8c0a4008c0b4008c0ac004cc024c8cc004004dd5980d18131baa00222533302800114bd7009981498131815000998010011815800810981398121baa00116330043758600260466ea8c020c08cdd500c119baf300930243754002601260486ea8c060c090dd50019181318139813800981218109baa00116330013758600a60406ea8c014c080dd500a919baf30063021375400202644646600200200644a666048002298103d87a8000132325333023300500213007330270024bd70099802002000981400118130009ba548000cc004dd6181018109810981098109810981098109810980e9baa3002301d375402400844646600200200644a66604200229404c94ccc07ccdc79bae302400200414a226600600600260480024603e002602800e6eb8c070c064dd5004899299980c1809980c9baa00a14a2264944dd6980e980d1baa009375a603860326ea8020dd6980d980e0011bad301a001301a002375c603000260300046eb4c058004c048dd5000980a0011809980a000998088051980880325eb80c044c038dd50008a503001300d375400446020602200229309b2b192999804980280089919299980718088010a4c2c6eb8c03c004c02cdd50010a99980498020008a99980618059baa00214985854ccc024cdc3a40080022a66601860166ea8008526161630093754002a66600c6004600e6ea800c4c8c8c8c8c8c8c8c8c8c94ccc04cc0580084c926325333011300d0011323253330163019002149858dd7180b80098099baa00215333011300c00115333014301337540042930b0b18089baa0011630140013014002375a602400260240046eb4c040004c040008dd7180700098070011bad300c001300837540062c6e1d2002370e90001bae0015734aae7555cf2ab9f5740ae855d11",
    [threadTokenPolicyId],
    { dataType: "list", items: [{ dataType: "bytes" }] }
  ),
  "PlutusV2"
);

const productPrice = 50n * 1_000_000n;

const productCollateral = 25n * 1_000_000n;

const threadTokenAsset = makeValue(productPrice + productCollateral, ...[[threadTokenUnit, 1n]]);

const minFee = 3n * 1_000_000n;

const data = {
  state: 1n,
  seller: "d0f4b0252c3c54d0ec21fe600c51489db9d5c534f14afc3227aa7af9",
  collateral: productCollateral,
  price: productPrice,
  buyer: "424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b",
};

const Datum = Data.Object({
  state: Data.Integer(),
  seller: Data.Bytes(),
  collateral: Data.Integer(),
  price: Data.Integer(),
  buyer: Data.Nullable(Data.Bytes()),
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
