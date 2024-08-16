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
      "59058c0100003232323232323223223232322322533300a3232533300c3007300d3754002264646464646464646464646464646464646464646464a666044603c60466ea802c4c8c8c94ccc094c084c098dd5000899192999813981198141baa0011323232533302a0081533302a0071533302a002100114a0294052819baf3374a900219816980b99816a610120003302d375201a6605a6ea002ccc0b4dd400499816a60103d87a80004bd7025eb80c060c0acdd500119809180c1bab301e302a375400204c605860526ea800458cc04cdd6180a98141baa30123028375403a466ebcc04cc0a4dd5000980998149baa301d302937540046054604e6ea800458cc044dd6180818131baa301030263754036466ebcc044c09cdd500080c998069bac300e30253754601e604a6ea806801cc0780204c8c8c8c8c8c8c8c8c94ccc0acc098c0b0dd500a0991919192999817981598181baa001132325333031302d30323754002264646464a66606a0122a66606a0102a66606a0042a66606a006200229405280a5014a066ebccdd2a40086607060446607098101010033038375201e660706ea0034cc0e0dd40059981c18111981c1ba90094bd7025eb812f5c06046606c6ea800ccdc480519299981a1817981a9baa0011480004dd6981c981b1baa001325333034302f303537540022980103d87a80001323300100137566074606e6ea8008894ccc0e4004530103d87a8000132323232533303a33722911000021533303a3371e910100002130283303e375000297ae014c0103d87a8000133006006003375a60760066eb8c0e4008c0f4008c0ec004c8cc004004dd59815181b1baa00322533303800114c103d87a800013232323253330393372291100002153330393371e910100002130273303d374c00297ae014c0103d87a8000133006006003375660740066eb8c0e0008c0f0008c0e8004cc070c088dd59814181a1baa0010303036303337540022c6603a6eb0c07cc0c8dd5180e18191baa02723375e603a60666ea8004c074c0ccdd5181398199baa0023034303137540022c660366eb0c068c0c0dd5180d18181baa02523375e603660626ea800408ccc05cdd6180c18179baa3019302f375404800460500126eb8c0c0c0b4dd500a099299981619b8748010c0b4dd500a8a511324a26eb4c0c4c0b8dd500a1bad3030302d3754026605e60600046eb4c0b8004c0b8008dd6981600098160011bae302a001302a002375a605000260486ea8028c098c09c008dd6981280098128011bad30230013023002375c604200260420046eb4c07c004c06cdd5000980e805980e180e80511191980080080191299980e8008a50132533301b3371e6eb8c08000801052889980180180098100009180d980e180e180e180e180e180e180e180e0009180d00091191980080080191299980d0008a60103d87a80001323253330193005002130073301d0024bd70099802002000980f001180e0009ba5480008c05cc060c0600048c008004c004004894ccc04c00452f5c02660286022602a00266004004602c002660220146602200c97ae03011300e37540022940c004c034dd50011180818088008a4c26cac64a666012600a0022a66601860166ea80085261615333009300400113232533300e3011002149858dd7180780098059baa002153330093370e90020008a99980618059baa00214985854ccc024cdc3a400c0022a66601860166ea8008526161630093754002a66600c6004600e6ea800c4c8c8c8c8c8c8c8c8c8c94ccc04cc0580084c926325333011300d0011323253330163019002149858dd7180b80098099baa00215333011300c00115333014301337540042930b0b18089baa0011630140013014002375a602400260240046eb4c040004c040008dd7180700098070011bad300c001300837540062c6e1d2002370e90001bae0015734aae7555cf2ab9f5740ae855d101",
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
  buyer: null,
};

const Datum = Data.Object({
  state: Data.Integer(),
  seller: Data.Bytes(),
  collateral: Data.Integer(),
  price: Data.Integer(),
  buyer: Data.Nullable(),
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
