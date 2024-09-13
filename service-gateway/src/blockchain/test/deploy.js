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
  projectId: "preprodkWGZGJyPVdkWvaCkGzqJRY2R2BM4Gopt",
});

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const validatorsWithParams = (tokenName, utxoRef) => {
  const threadTokenScript = cborToScript(
    applyParamsToScript(
      "5901ae01010032323232323232232225333005323232323253323300b3001300c3754004264646464a66601e600a0022a66602460226ea801c540085854ccc03cc00c00454ccc048c044dd50038a8010b0b18079baa006132323232533301430170021323253330133009301437540162a666026601260286ea8c8cc004004018894ccc0600045300103d87a80001323253330173375e603860326ea80080504cdd2a40006603600497ae0133004004001301c002301a00115333013300700113371e00402229405854ccc04ccdc3800a4002266e3c0080445281bad3014002375c60240022c602a00264a666020600860226ea800452f5bded8c026eacc054c048dd500099198008009bab3015301630163016301600322533301400114c103d87a80001323232325333015337220140042a66602a66e3c0280084cdd2a4000660326e980052f5c02980103d87a80001330060060033756602c0066eb8c050008c060008c058004dd6180980098079baa007370e90011bae3010300d37540046e1d200016300e300f002300d001300d002300b0013007375400229309b2b1bae0015734aae7555cf2ab9f5740ae855d11",
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
      "5902dc0101003232323232323223225333004323232323253330093370e900118051baa0011323232323232533300f300300115333012301137540102a0042c2a66601e66e1d2002001132323232533301630190021500616375a602e002602e0046eb8c054004c044dd50040a99980799b874801000454ccc048c044dd50040a8010b0a99980799b874801800454ccc048c044dd50040a8010b0a99980799b874802000454ccc048c044dd50040a8010b0b18079baa0071533300d3001300e375400426464646464646464a66602a6012602c6ea8c068c06c0204c8c8c8c8c8c94ccc06cc03cc070dd5000899191919299980f980998101baa0011323232533302200d1533302200b15333022002100114a0294052819baf3374a90021981299ba548000cc09530010120003302537520206604a6ea0038cc094dd400699812a60103d87a8000330254c103d87a80004bd7025eb80c010c08cdd50011980519198008009bab30053023375400444a66604a002297ae01330263023302700133002002302800101e3024302137540022c6600a6eb0c004c080dd500c919baf300730213754002600e60426ea8c00cc084dd500211811981218120009181118118009810180e9baa00116330013758600460386ea80548cdd79801980e9baa00101122323300100100322533302000114c103d87a800013232533301f300500213374a90001981180125eb804cc010010004c090008c0880048c078004cc004dd6180e180e980e980e980e980e980e980e980e980c9baa01200622323300100100322533301d00114a0264a66603666e3cdd718100010020a511330030030013020001300900614a06eb4c064c068008dd6980c000980c0011bae30160013016002375a602800260206ea8c04c004cc044c048c03cdd50011980880325eb8058dc3a400060206022004601e00260166ea800458c034c038008c030004c030008c028004c018dd50008a4c26cac6eb80055cd2ab9d5573caae7d5d02ba157441",
      [threadTokenPolicyId],
      {
        dataType: "list",
        items: [{ dataType: "bytes" }],
      }
    ),
    "PlutusV3"
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
  "addr_test1qpqzsucnvps0v44cpqk8j74gqhkgwznckkw4yq4rt5szf0wzyv3wg0ry38rw8a86dey7j2mxfp72j298ygcxzawstrlsk6d79p"
);

const wallet = new ColdWallet(externalWallet, Core.NetworkId.Testnet, provider);

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
  seller: "402873136060f656b8082c797aa805ec870a78b59d5202a35d2024bd",
  collateral: productCollateral,
  price: productPrice,
  buyer: null,
  range: null
};

const Datum = Data.Object({
  state: Data.Integer(),
  seller: Data.Bytes(),
  collateral: Data.Integer(),
  price: Data.Integer(),
  buyer: Data.Nullable(),
  range: Data.Nullable(),
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
  .addRequiredSigner("402873136060f656b8082c797aa805ec870a78b59d5202a35d2024bd")
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
