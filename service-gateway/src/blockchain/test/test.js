import {
  Lucid,
  Blockfrost,
  Constr,
  fromText,
  validatorToScriptHash,
  validatorToAddress,
  Data,
  applyDoubleCborEncoding,
} from "@lucid-evolution/lucid";
import { applyParamsToScript } from "@lucid-evolution/utils";

import blueprint from "./plutus.json" assert { type: "json" };

const lucid = await Lucid(
  new Blockfrost(
    "https://cardano-preprod.blockfrost.io/api/v0",
    "preprodkWGZGJyPVdkWvaCkGzqJRY2R2BM4Gopt"
  ),
  "Preprod"
);

const getValidators = () => {
  const threadToken = blueprint.validators.find(
    (v) => v.title === "marketplace.threadtoken.mint"
  );

  if (!threadToken) {
    throw new Error("threadToken validator not found");
  }

  const stateMachine = blueprint.validators.find(
    (v) => v.title === "marketplace.statemachine.spend"
  );

  if (!stateMachine) {
    throw new Error("stateMachine validator not found");
  }

  return {
    threadToken: {
      type: "PlutusV3",
      script: applyDoubleCborEncoding(threadToken.compiledCode),
    },
    stateMachine: {
      type: "PlutusV3",
      script: applyDoubleCborEncoding(stateMachine.compiledCode),
    },
  };
};

const validators = getValidators();

const validatorWithParams = (tokenName, outRef) => {
  const threadToken = applyParamsToScript(validators.threadToken.script, [
    tokenName,
    outRef,
  ]);

  const threadTokenPolicyId = validatorToScriptHash({
    type: "PlutusV3",
    script: threadToken,
  });

  const stateMachine = applyParamsToScript(validators.stateMachine.script, [
    threadTokenPolicyId,
  ]);

  const stateMachineAddress = validatorToAddress("Preprod", {
    type: "PlutusV3",
    script: stateMachine,
  });

  return {
    threadToken: {
      type: "PlutusV3",
      script: threadToken,
    },
    threadTokenPolicyId,
    stateMachine: {
      type: "PlutusV3",
      script: stateMachine,
    },
    stateMachineAddress,
  };
};
/////////////////////////////////////////////////////////////

const externalWallet =
  "addr_test1qpqzsucnvps0v44cpqk8j74gqhkgwznckkw4yq4rt5szf0wzyv3wg0ry38rw8a86dey7j2mxfp72j298ygcxzawstrlsk6d79p";

const externalUtxos = await lucid.utxosAt(externalWallet);

lucid.selectWallet.fromAddress(externalWallet, externalUtxos);

const utxo = externalUtxos[0];

const outRef = new Constr(0, [
  new Constr(0, [utxo.txHash]),
  BigInt(utxo.outputIndex),
]);

const tokenName = "threadtoken";

const assetName = fromText(tokenName);

const parameterizedValidators = validatorWithParams(assetName, outRef);

const policyId = parameterizedValidators.threadTokenPolicyId;

const threadTokenUnit = policyId + assetName;

const threadTokenInput = Data.to(new Constr(0, []));

const productCollateral = 25n * 1_000_000n;

const productPrice = 50n * 1_000_000n;

const data = {
  state: 0n,
  seller: "402873136060f656b8082c797aa805ec870a78b59d5202a35d2024bd",
  collateral: productCollateral,
  price: productPrice,
  buyer: null,
  range: null,
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

const tx = await lucid
  .newTx()
  .collectFrom([utxo])
  .mintAssets(
    {
      [threadTokenUnit]: 1n,
    },
    threadTokenInput
  )
  .attach.MintingPolicy(parameterizedValidators.threadToken)
  .addSignerKey("402873136060f656b8082c797aa805ec870a78b59d5202a35d2024bd")
  .pay.ToContract(
    parameterizedValidators.stateMachineAddress,
    { kind: "inline", value: stateMachineDatum },
    { lovelace: 10_000_000n, [threadTokenUnit]: 1n }
  )
  .complete({
    changeAddress: externalWallet,
  });

const transaction = await tx.toCBOR();

console.log(transaction);
