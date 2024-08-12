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
      "5903cb0100003232323232323223223232232322533300a3232533300c3005300d375400226464646464646464646464a66602e602660306ea80244c8c8c8c8c8c8c94ccc078c068c07cdd500089919192999810980e98111baa001132323232533302500e1533302500b1533302500215333025003100114a029405280a503375e66e952004330283009330284c01010100330283752022660506ea003ccc0a0dd400725eb812f5c0600a604c6ea800ccdc4807192999812180e98129baa0011480004dd6981498131baa001325333024301d302537540022980103d87a80001323300100137566054604e6ea8008894ccc0a4004530103d87a8000132323232533302a33722911000021533302a3371e9101000021300f3302e375000297ae014c0103d87a8000133006006003375a60560066eb8c0a4008c0b4008c0ac004c8cc004004dd5980d18131baa00322533302800114c103d87a800013232323253330293372291100002153330293371e9101000021300e3302d374c00297ae014c0103d87a8000133006006003375660540066eb8c0a0008c0b0008c0a8004cc024c8cc004004dd5980c98129baa00222533302700114bd7009981418129814800998010011815000810181318119baa00116330043758600260446ea8c020c088dd500b919baf300930233754002601260466ea8c05cc08cdd50019181298131813000981198101baa00116330013758600a603e6ea8c014c07cdd500a119baf30063020375400202444646600200200644a666046002298103d87a8000132325333022300500213007330260024bd70099802002000981380118128009ba548000cc004dd6180f98101810181018101810181018101810180e1baa3002301c375402200e44646600200200644a66604000229404c94ccc078cdc79bae302300200414a226600600600260460024603c002602600c264a666030602260326ea8028528899251375a603a60346ea8024dd6980e180c9baa008375a603660380046eb4c068004c068008dd7180c000980c0011bad301600130123754002602800460266028002660220146602200e97ae03011300e37540022940c004c034dd50011180818088008a4c26cac64a666012600a0022a66601860166ea800c526161533300930020011533300c300b37540062930b0a99980499b874801000454ccc030c02cdd50018a4c2c2c60126ea8008dc3a4004a66600a6002600c6ea80084c8c8c8c8c8c8c8c94ccc040c04c00852616375a602200260220046eb4c03c004c03c008dd7180680098068011bad300b001300737540042c6e1d2000375c002ae6955ceaab9e5573eae815d0aba21",
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
