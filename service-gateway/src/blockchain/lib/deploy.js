import {
  ColdWallet,
  Core,
  Data,
  Blaze,
  makeValue,
  Blockfrost,
  cborToScript,
  applyParamsToScript,
} from "@blaze-cardano/sdk";

const provider = new Blockfrost({
  network: "cardano-preprod",
  projectId: "preprodex26NYImZOT84XAA67qhyHyA7TT6PCGI",
});

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const validatorsWithParams = (tokenName, utxoRef) => {
  const threadTokenScript = cborToScript(
    applyParamsToScript(
      "5901f5010000323232323232322322232323225333009323232533300c3007300d3754002264646464a666026602c00426464a666024601a60266ea803854ccc048c034c04cdd5191980080080311299980b8008a60103d87a80001323253330163375e603660306ea800804c4cdd2a40006603400497ae0133004004001301b002301900115333012300c00113371e00402029405854ccc048cdc3800a4002266e3c0080405281bad3013002375c60220022c602800264a66601e601260206ea800452f5bded8c026eacc050c044dd500099191980080099198008009bab3016301730173017301700522533301500114bd6f7b630099191919299980b19b91488100002153330163371e9101000021003100513301a337606ea4008dd3000998030030019bab3017003375c602a0046032004602e00244a666028002298103d87a800013232323253330153372200e0042a66602a66e3c01c0084cdd2a4000660326e980052f5c02980103d87a80001330060060033756602c0066eb8c050008c060008c058004dd7180998081baa00337586024002601c6ea800858c040c044008c03c004c02cdd50008a4c26cac64a66601060060022a66601660146ea8010526161533300830020011533300b300a37540082930b0b18041baa003370e90011b8748000dd7000ab9a5573aaae7955cfaba05742ae89",
      [tokenName, utxoRef],
      {
        dataType: "list",
        items: [
          { dataType: "bytes" },
          {
            title: "OutputReference",
            description:
              "An `OutputReference` is a unique reference to an output on-chain. The `output_index`\n corresponds to the position in the output list of the transaction (identified by its id)\n that produced that output",
            anyOf: [
              {
                title: "OutputReference",
                dataType: "constructor",
                index: 0,
                fields: [
                  {
                    title: "transactionId",
                    description:
                      "A unique transaction identifier, as the hash of a transaction body. Note that the transaction id\n isn't a direct hash of the `Transaction` as visible on-chain. Rather, they correspond to hash\n digests of transaction body as they are serialized on the network.",
                    anyOf: [
                      {
                        title: "TransactionId",
                        dataType: "constructor",
                        index: 0,
                        fields: [{ dataType: "bytes", title: "hash" }],
                      },
                    ],
                  },
                  { dataType: "integer", title: "outputIndex" },
                ],
              },
            ],
          },
        ],
      }
    ),
    "PlutusV2"
  );
  const threadTokenPolicyId = threadTokenScript.hash();

  const stateMachineScript = cborToScript(
    applyParamsToScript(
      "5904390100003232323232323223223232322322533300a3232533300c3007300d3754002264646464646464646464646464a666032602a60346ea802c4c8c8c8c8c8c8c8c94ccc084c074c088dd500089919192999812181018129baa001132323232533302800e1533302800b1533302800215333028003100114a029405280a503375e66e9520043302b30093302b4c010101003302b3752028660566ea0048cc0acdd4008198159804998159ba900e4bd7025eb812f5c0600a60526ea800ccdc4807992999813981118141baa0011480004dd6981618149baa0013253330273022302837540022980103d87a8000132330010013756605a60546ea8008894ccc0b0004530103d87a8000132323232533302d33722911000021533302d3371e9101000021300f33031375000297ae014c0103d87a8000133006006003375a605c0066eb8c0b0008c0c0008c0b8004c8cc004004dd5980e98149baa00322533302b00114c103d87a8000132323232533302c33722911000021533302c3371e9101000021300e33030374c00297ae014c0103d87a80001330060060033756605a0066eb8c0ac008c0bc008c0b4004cc024c8cc004004dd5980e18141baa00222533302a00114bd7009981598141816000998010011816800811981498131baa001163300437586002604a6ea8c020c094dd500d119baf3009302637540026012604c6ea8c068c098dd50019181418149814800981318119baa00116330013758600a60446ea8c014c088dd500b919baf30063023375400202a44646600200200644a66604c002298103d87a8000132325333025300500213007330290024bd70099802002000981500118140009ba548000cc004dd6181118119811981198119811981198119811980f9baa3002301f375402800844646600200200644a66604600229404c94ccc084cdc79bae302600200414a2266006006002604c00246042002602c0126eb8c078c06cdd5005899299980d180a980d9baa00c14a2264944dd6980f980e1baa00b375a603c60366ea8028c074c078008dd6980e000980e0011bad301a001301a002375c603000260300046eb4c058004c048dd5000980a0011809980a000998088051980880325eb80c044c038dd50008a503001300d375400446020602200229309b2b192999804980280089919299980718088010a4c2c6eb8c03c004c02cdd50010a99980498020008a99980618059baa00214985854ccc024cdc3a40080022a66601860166ea8008526161630093754002a66600c6004600e6ea800c4c8c8c8c8c8c8c8c8c8c94ccc04cc0580084c926325333011300d0011323253330163019002149858dd7180b80098099baa00215333011300c00115333014301337540042930b0b18089baa0011630140013014002375a602400260240046eb4c040004c040008dd7180700098070011bad300c001300837540062c6e1d2002370e90001bae0015734aae7555cf2ab9f5740ae855d11",
      [threadTokenPolicyId],
      {
        dataType: "list",
        items: [{ dataType: "bytes" }],
      }
    ),
    "PlutusV2"
  );

  const stateMachineAddress = Core.addressFromValidator(
    Core.NetworkId.Testnet,
    stateMachineScript
  );

  return {
    threadTokenScript,
    stateMachineScript,
    threadTokenPolicyId,
    stateMachineAddress,
  };
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const externalWallet = Core.addressFromBech32(
  "addr_test1qrg0fvp99s79f58vy8lxqrz3fzwmn4w9xnc54lpjy74847v04wk5sd4fhk5jur50npqse22mjn4we4r4l7uxfpdggrcsf7cf5y"
);

const wallet = new ColdWallet(externalWallet, 2, provider);

const blaze = await Blaze.from(provider, wallet);

const externalWalletUtxos = await blaze.provider.getUnspentOutputs(
  externalWallet
);

if (externalWalletUtxos.length < 1) {
  throw new Error("WALLET_NO_UTXOS");
}

const utxo = externalWalletUtxos[0];

const outRef = {
  transactionId: { hash: utxo.input().transactionId() },
  outputIndex: utxo.input().index(),
};

const tokenName = "threadtoken";

const assetName = Buffer.from(tokenName, "utf8").toString("hex");

const parameterizedValidators = validatorsWithParams(assetName, outRef);

const policyId = parameterizedValidators.threadTokenPolicyId;

const threadTokenUnit = policyId + assetName;

const productCollateral = 25n * 1_000_000n;

const productPrice = 50n * 1_000_000n;

const data = {
  state: 0n,
  seller: "d0f4b0252c3c54d0ec21fe600c51489db9d5c534f14afc3227aa7af9",
  collateral: productCollateral,
  price: productPrice,
  buyer: null
};

const Datum = Data.Object({
  state: Data.Integer(),
  seller: Data.Bytes(),
  collateral: Data.Integer(),
  price: Data.Integer(),
  buyer: Data.Nullable()
});

const stateMachineDatum = Data.to(data, Datum);

const threadTokenInput = Core.PlutusData.newConstrPlutusData(
  new Core.ConstrPlutusData(0n, new Core.PlutusList())
);

const threadTokenAsset = makeValue(
  productCollateral,
  ...[[threadTokenUnit, 1n]]
);

const tokenMap = new Map();

tokenMap.set(assetName, 1n);

const minFee = 1_000_000n;

const tx = await blaze
  .newTransaction()
  .addInput(utxo)
  .addMint(policyId, tokenMap, threadTokenInput)
  .provideScript(parameterizedValidators.threadTokenScript)
  .lockAssets(
    parameterizedValidators.stateMachineAddress,
    threadTokenAsset,
    stateMachineDatum
  )
  .addRequiredSigner("d0f4b0252c3c54d0ec21fe600c51489db9d5c534f14afc3227aa7af9")
  .setChangeAddress(externalWallet)
  .setMinimumFee(minFee)
  .complete();

const cbor = tx.toCbor();

console.log("CBOR: " + cbor);

console.log("policyId: " + policyId);

console.log("threadTokenUnit: " + threadTokenUnit);

console.log(
  "stateMachineAddress: " +
    parameterizedValidators.stateMachineAddress.toBech32()
);
///////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////
const protocolParameters = await blaze.provider.getParameters();
const price = protocolParameters.prices;
const redeemers = await provider.evaluateTransaction(tx);

console.log("\nprices: " + JSON.stringify(price));

for (const redeemer of redeemers.values()) {
  const memory = redeemer.exUnits().mem();
  const steps = redeemer.exUnits().steps();
  const memoryPrice = Number(memory) * price.memory;
  const cpuPrice = Number(steps) * price.steps;
  const total = parseInt(memoryPrice + cpuPrice) / 1000000;
  const result = total + protocolParameters.minFeeConstant;
  console.log("memory: ", memory, memoryPrice);
  console.log("cpu: ", steps, cpuPrice);
  console.log("protocolMinFee", protocolParameters.minFeeConstant);
  console.log("total: ", result);
}
