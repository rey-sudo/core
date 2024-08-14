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
  "addr_test1wp26yngfdpf2twy4zk2ph4wxj2d5kg9zkchscs0zwmcexqq9m3erq"
);

const wallet = new ColdWallet(externalWallet, 2, provider);

const blaze = await Blaze.from(provider, wallet);

///////////////////////////////////

const threadTokenUnit =
  "16310f52df43c355c2b6dd83cd842824051d2d398d64a47ee3f1fe87746872656164746f6b656e";

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
  Data.Object({ Locking: Data.Object({ buyer_param: Data.Bytes() }) }),
  Data.Literal("Delivered"),
  Data.Literal("Received"),
]);

const lockingInput = { Locking: { buyer_param: "424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b" } };

const stateMachineRedeemer = Data.to(lockingInput, stateMachineInput);


const stateMachineScript = cborToScript(
  applyParamsToScript(
    "5904390100003232323232323223223232322322533300a3232533300c3007300d3754002264646464646464646464646464a666032602a60346ea802c4c8c8c8c8c8c8c8c94ccc084c074c088dd500089919192999812181018129baa001132323232533302800e1533302800b1533302800215333028003100114a029405280a503375e66e9520043302b30093302b4c010101003302b3752028660566ea0048cc0acdd4008198159804998159ba900e4bd7025eb812f5c0600a60526ea800ccdc4807992999813981118141baa0011480004dd6981618149baa0013253330273022302837540022980103d87a8000132330010013756605a60546ea8008894ccc0b0004530103d87a8000132323232533302d33722911000021533302d3371e9101000021300f33031375000297ae014c0103d87a8000133006006003375a605c0066eb8c0b0008c0c0008c0b8004c8cc004004dd5980e98149baa00322533302b00114c103d87a8000132323232533302c33722911000021533302c3371e9101000021300e33030374c00297ae014c0103d87a80001330060060033756605a0066eb8c0ac008c0bc008c0b4004cc024c8cc004004dd5980e18141baa00222533302a00114bd7009981598141816000998010011816800811981498131baa001163300437586002604a6ea8c020c094dd500d119baf3009302637540026012604c6ea8c068c098dd50019181418149814800981318119baa00116330013758600a60446ea8c014c088dd500b919baf30063023375400202a44646600200200644a66604c002298103d87a8000132325333025300500213007330290024bd70099802002000981500118140009ba548000cc004dd6181118119811981198119811981198119811980f9baa3002301f375402800844646600200200644a66604600229404c94ccc084cdc79bae302600200414a2266006006002604c00246042002602c0126eb8c078c06cdd5005899299980d180a980d9baa00c14a2264944dd6980f980e1baa00b375a603c60366ea8028c074c078008dd6980e000980e0011bad301a001301a002375c603000260300046eb4c058004c048dd5000980a0011809980a000998088051980880325eb80c044c038dd50008a503001300d375400446020602200229309b2b192999804980280089919299980718088010a4c2c6eb8c03c004c02cdd50010a99980498020008a99980618059baa00214985854ccc024cdc3a40080022a66601860166ea8008526161630093754002a66600c6004600e6ea800c4c8c8c8c8c8c8c8c8c8c94ccc04cc0580084c926325333011300d0011323253330163019002149858dd7180b80098099baa00215333011300c00115333014301337540042930b0b18089baa0011630140013014002375a602400260240046eb4c040004c040008dd7180700098070011bad300c001300837540062c6e1d2002370e90001bae0015734aae7555cf2ab9f5740ae855d11",
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
