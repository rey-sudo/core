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

const validatorsWithParams = (tokenName, outRef) => {
  const threadTokenScript = cborToScript(
    applyParamsToScript(
      "5901f5010000323232323232322322232323225333009323232533300c3007300d3754002264646464a666026602c00426464a666024601a60266ea803854ccc048c034c04cdd5191980080080311299980b8008a60103d87a80001323253330163375e603660306ea800804c4cdd2a40006603400497ae0133004004001301b002301900115333012300c00113371e00402029405854ccc048cdc3800a4002266e3c0080405281bad3013002375c60220022c602800264a66601e601260206ea800452f5bded8c026eacc050c044dd500099191980080099198008009bab3016301730173017301700522533301500114bd6f7b630099191919299980b19b91488100002153330163371e9101000021003100513301a337606ea4008dd3000998030030019bab3017003375c602a0046032004602e00244a666028002298103d87a800013232323253330153372200e0042a66602a66e3c01c0084cdd2a4000660326e980052f5c02980103d87a80001330060060033756602c0066eb8c050008c060008c058004dd7180998081baa00337586024002601c6ea800858c040c044008c03c004c02cdd50008a4c26cac64a66601060060022a66601660146ea8010526161533300830020011533300b300a37540082930b0b18041baa003370e90011b8748000dd7000ab9a5573aaae7955cfaba05742ae89",
      [tokenName, outRef],
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
      "5903f50100003232323232323223223232232322533300a3232533300c3005300d37540022646464646464646464a66602a6022602c6ea801c4c8c8c8c8c8c8c94ccc070c060c074dd50008991919299980f980d98101baa001132323232533302300e1533302300b1533302300215333023003100114a029405280a503375e66e952004330263009330264c0101010033026375201e6604c6ea00392f5c097ae030053024375400666e24034c94ccc088c06cc08cdd50008a400026eb4c09cc090dd5000992999811180d98119baa00114c0103d87a80001323300100137566050604a6ea8008894ccc09c004530103d87a800013232323253330283372291100002153330283371e9101000021300f3302c375000297ae014c0103d87a8000133006006003375a60520066eb8c09c008c0ac008c0a4004c8cc004004dd5980c18121baa00322533302600114c103d87a800013232323253330273372291100002153330273371e9101000021300e3302b374c00297ae014c0103d87a8000133006006003375660500066eb8c098008c0a8008c0a0004cc024c8cc004004dd5980b98119baa00222533302500114bd700998131811981380099801001181400080f181218109baa00116330043758600260406ea8c020c080dd500a919baf300930213754002601260426ea8c054c084dd500191811981218120009810980f1baa00116330013758600a603a6ea8c014c074dd5009119baf3006301e375400202044646600200200644a666042002298103d87a8000132325333020300500213007330240024bd70099802002000981280118118009ba548000cc004dd6180e980f180f180f180f180f180f180f180f180d1baa3002301a375401e00a44646600200200644a66603c00229404c94ccc070cdc79bae302100200414a22660060060026042002460380026022008264a66602c601e602e6ea80204c9289bae301b30183754010264a66602e66e1d20043018375401229444c9289bad301c301937540106eb4c06cc060dd50039bad301a3017375400c6eb4c064c068008dd7180c000980c0011bad301600130123754002602800460266028002660220146602200e97ae03011300e37540022940c004c034dd50011180818088008a4c26cac64a666012600a0022a66601860166ea800c5261615333009300200113232533300e3011002149858dd7180780098059baa003153330093370e90020008a99980618059baa00314985854ccc024cdc3a400c0022a66601860166ea800c5261616300937540046e1d200253330053001300637540042646464646464a66601c60220042930b1bad300f001300f002375c601a002601a0046eb4c02c004c01cdd50010b1b8748000dd7000ab9a5573aaae7955cfaba05742ae881",
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
  "addr_test1qppygdhzm0t7nnlclmds3dy0wc3du870dpy48juu0xxuu2aefdfvc4e0785y7vfhwlmsn3rn26mzvv9md0mhnkpjlc4s0jshh4"
);

const wallet = new ColdWallet(externalWallet, 2, provider);

const blaze = await Blaze.from(provider, wallet);

const protocolParameters = await blaze.provider.getParameters();

console.log(protocolParameters);

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

const data = {
  state: 0n,
  seller: "424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b",
  collateral: productCollateral,
};

const Datum = Data.Object({
  state: Data.Integer(),
  seller: Data.Bytes(),
  collateral: Data.Integer(),
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

const minFee = BigInt(minFeeConstant);

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
  .addRequiredSigner("424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b")
  .setChangeAddress(externalWallet)
  .setMinimumFee(minFee)
  .complete();

const cbor = tx.toCbor();

console.log(cbor);

console.log("policyId: " + policyId);

console.log("threadTokenUnit: " + threadTokenUnit);

console.log(
  "stateMachineAddress: " +
    parameterizedValidators.stateMachineAddress.toBech32()
);
