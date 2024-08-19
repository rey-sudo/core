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
      "5907f2010000323232323232322322323232232322533300b3232533300d3008300e375400226464646464646464646464646464646464646464a64666044603c60466ea80284c8c8c94ccc094c084c098dd5000899192999813981198141baa0011323232533302a0081533302a0071533302a002100114a0294052819baf30083302d30163302d4c010120003302d37520186605a6ea0028cc0b4dd400499816a60103d87a80003302d4c103d87a80004bd7025eb80c05cc0acdd500119808980b9bab301d302a375400204c605860526ea800458cc048dd6180a18141baa301130283754038466ebcc048c0a4dd5000980918149baa301c302937540046054604e6ea800458cc040dd6180798131baa300f30263754034466ebcc040c09cdd500080c198061bac300d30253754601c604a6ea8064018c07801c4c8c8c8c8c8c8c8c8c8c94ccc0b0c09cc0b4dd500a09919191919191929998199817981a1baa001132325333035303130363754002264646464a6660720142a6660720122a6660720102a6660720062a666072004200229405280a5014a02940cdd7980b9981e18129981e26010101003303c3752024660786ea0040cc0f0dd40071981e18129981e1ba900b4bd701981e18129981e1ba800a4bd7025eb812f5c0604c60746ea800ccdc4806980a9bab302c303937540046603e604a6eacc0acc0e0dd500081a181d181b9baa001163302037586044606c6ea8c07cc0d8dd5015119baf3020303737540026040606e6ea8c0a8c0dcdd5001181c181a9baa001163301e3758603a60686ea8c074c0d0dd5014119baf301e3035375400204c660346eb0c06cc0ccdd5180e18199baa0270043375e00a980103d87a8000302b00b375a606660680046eb8c0c8004c0b8dd500a09919191919191919191919299981b9817981c1baa01f132323232533303b3037303c375400226464a66607a6072607c6ea80044c8c8c8c94ccc10402854ccc10402454ccc10402054ccc10400c54ccc10400840045280a5014a0294052819baf301f33044302d330444c01010000330443752022660886ea003ccc110dd400699822260103d87a8000330444c103d87a80004bd7025eb80c0b8c108dd500199b87301d3756606860826ea8008038cc09cc0b4dd5981998201baa00103c3042303f37540022c660506eb0c0a8c0f8dd51813981f1baa03223375e6050607e6ea8004c0a0c0fcdd51819181f9baa0023040303d37540022c6604c6eb0c094c0f0dd51812981e1baa03023375e604c607a6ea80040b8c94ccc0e8c0d4c0ecdd5000899299981d981b981e1baa0051337126eb4c100c0f4dd50028008a50375a607e60786ea80045281812181d9baa3024303b3754607c607e607e607e607e607e607e607e60766ea8c090c0ecdd5017a99981c181a181c9baa0031330213758604460746ea8c08cc0e8dd50171bae303d303a37540062940c0c80284c94ccc0e0cdc3a400c60726ea80804c0cc0044c0c0dd6981e981d1baa01f375a607860726ea8078c0ecc0f0008c0e8004c0e8008dd6981c000981c0011bad30360013036002375c606800260680046eb4c0c8004c0b8dd5009981818188011bad302f001302f002375a605a002605a0046eb8c0ac004c0ac008dd6981480098129baa00a2325333024301f302537540022900009bad30293026375400264a666048603e604a6ea80045300103d87a80001323300100137566054604e6ea8008894ccc0a4004530103d87a8000132323232533302a33722911000021533302a3371e910100002130173302e375000297ae014c0103d87a8000133006006003375a60560066eb8c0a4008c0b4008c0ac004c8cc004004008894ccc0a00045300103d87a800013232323253330293372291100002153330293371e910100002130163302d374c00297ae014c0103d87a8000133006006003375660540066eb8c0a0008c0b0008c0a8004dd2a40086eb4c094c098008dd6981200098120011bae30220013022002375a604000260386ea8004c07802cc074c07802888c8cc00400400c894ccc078004528099299980e19b8f375c604200400829444cc00c00c004c0840048c070c074c074c074c074c074c074c074c0740048c06c00488c8cc00400400c894ccc06c0045300103d87a800013232533301a3005002130073301e0024bd70099802002000980f801180e8009ba5480008c060c064c0640048c008004c004004894ccc05000452f5c026602a6024602c00266004004602e002660240166602400e97ae03012300f37540022940c004c038dd50011180898090008a4c26cac64a666014600c0022a66601a60186ea800c526161533300a300500113232323253330113014002149858dd6980900098090011bae3010001300c37540062a66601460040022a66601a60186ea800c526161533300a3370e90030008a99980698061baa00314985854ccc028cdc3a40100022a66601a60186ea800c5261616300a37540046e1d200453330063002300737540062646464646464646464646464a66602a6030004264649319299980a180800089919299980c980e0010a4c2c6eb4c068004c058dd50018a99980a18078008a99980b980b1baa00314985858c050dd5001192999809980780089919299980c180d8010a4c2c6eb8c064004c054dd50020a99980998070008a99980b180a9baa00414985858c04cdd50018b180b000980b001180a000980a0011bad30120013012002375a602000260200046eb8c038004c038008dd6980600098041baa00316370e90011b8748000dd7000ab9a5573aaae7955cfaba05742ae89",
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
