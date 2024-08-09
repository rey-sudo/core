import {
  applyParamsToScript,
  Blockfrost,
  C,
  Constr,
  Data,
  fromHex,
  fromText,
  Lucid,
  MintingPolicy,
  SpendingValidator,
  toHex,
  TxHash,
  Unit,
} from "https://deno.land/x/lucid@0.10.7/mod.ts";
import * as cbor from "https://deno.land/x/cbor@v1.4.1/index.js";

const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preprod.blockfrost.io/api/v0",
    "preprodex26NYImZOT84XAA67qhyHyA7TT6PCGI",
  ),
  "Preprod",
);

lucid.selectWalletFromPrivateKey(await Deno.readTextFile("./preprod.sk"));

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const blueprint = JSON.parse(await Deno.readTextFile("plutus.json"));

type Validators = {
  threadToken: MintingPolicy;
  stateMachine: SpendingValidator;
};

const readValidators = (): Validators => {
  const threadToken = blueprint.validators.find(
    (v: any) => v.title === "marketplace.threadtoken",
  );

  if (!threadToken) {
    throw new Error("threadToken validator not found");
  }

  const stateMachine = blueprint.validators.find((v: any) =>
    v.title === "marketplace.machinestate"
  );

  if (!stateMachine) {
    throw new Error("stateMachine validator not found");
  }

  return {
    stateMachine: {
      type: "PlutusV2",
      script: stateMachine.compiledCode,
    },
    threadToken: {
      type: "PlutusV2",
      script: threadToken.compiledCode,
    },
  };
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const validators = await readValidators();

const validatorsWithParams = (tokenName: string, outRef: Data) => {
  const threadToken = applyParamsToScript(validators.threadToken.script, [
    fromText(tokenName),
    outRef,
  ]);

  const threadTokenPolicyId = lucid.utils.validatorToScriptHash({
    type: "PlutusV2",
    script: threadToken,
  });

  const stateMachine = applyParamsToScript(validators.stateMachine.script, [
    threadTokenPolicyId,
  ]);

  const stateMachineAddress = lucid.utils.validatorToAddress({
    type: "PlutusV2",
    script: stateMachine,
  });

  return {
    threadToken: {
      type: "PlutusV2",
      script: threadToken,
    },
    stateMachine: {
      type: "PlutusV2",
      script: stateMachine,
    },
    threadTokenPolicyId,
    stateMachineAddress,
  };
};

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

const localWalletUtxos = await lucid?.wallet.getUtxos()!;

const utxo = localWalletUtxos[0];

const outRef = new Constr(0, [
  new Constr(0, [utxo.txHash]),
  BigInt(utxo.outputIndex),
]);

const tokenName = "threadtoken";

const parameterizedValidators = validatorsWithParams(tokenName, outRef);

const mintRedeemer = Data.to(new Constr(0, []));

const policyId = parameterizedValidators.threadTokenPolicyId;

const stateMachineUnit: Unit = policyId + fromText(tokenName);

const datum = Data.to(
  new Constr(0, [
    BigInt(0),
    "424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b",
  ]),
);

const minUtxoLovelace = 2n * 1_000_000n;

const tx = await lucid
  .newTx()
  .collectFrom([utxo])
  .attachMintingPolicy(parameterizedValidators.threadToken as MintingPolicy)
  .mintAssets(
    { [stateMachineUnit]: BigInt(1) },
    mintRedeemer,
  )
  .payToContract(
    parameterizedValidators.stateMachineAddress,
    { inline: datum },
    {
      [stateMachineUnit]: BigInt(1),
      lovelace: BigInt(minUtxoLovelace),
    },
  )
  .complete();

const signedTx = await tx.sign().complete();

const txHash = await signedTx.submit();

console.log(txHash);

console.log("policyId: " + policyId);

console.log("stateMachineUnit: " + stateMachineUnit);
