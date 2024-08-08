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
} from "https://deno.land/x/lucid@0.10.7/mod.ts";
import * as cbor from "https://deno.land/x/cbor@v1.4.1/index.js";

const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preprod.blockfrost.io/api/v0",
    "preprodex26NYImZOT84XAA67qhyHyA7TT6PCGI",
  ),
  "Preprod",
);

const blueprint = JSON.parse(await Deno.readTextFile("plutus.json"));

export type Validators = {
  threadToken: MintingPolicy;
  machineState: SpendingValidator;
};

export function readValidators(): Validators {
  const threadToken = blueprint.validators.find(
    (v: any) => v.title === "marketplace.threadtoken",
  );

  if (!threadToken) {
    throw new Error("threadToken validator not found");
  }

  const machineState = blueprint.validators.find((v: any) =>
    v.title === "marketplace.machinestate"
  );

  if (!machineState) {
    throw new Error("machineState validator not found");
  }

  return {
    machineState: {
      type: "PlutusV2",
      script: machineState.compiledCode,
    },
    threadToken: {
      type: "PlutusV2",
      script: threadToken.compiledCode,
    },
  };
}

//////////////////////////////////////////////////////////////

lucid.selectWalletFromPrivateKey(await Deno.readTextFile("./preprod.sk"));

const validators = await readValidators();

const validatorWithParams = (tokenName: string, outRef: Data) => {
  const threadToken = applyParamsToScript(validators.threadToken.script, [
    fromText(tokenName),
    outRef,
  ]);

  const threadTokenPolicyId = lucid.utils.validatorToScriptHash({
    type: "PlutusV2",
    script: threadToken,
  });

  const machineStateAddress = lucid.utils.validatorToAddress({
    type: "PlutusV2",
    script: validators.machineState.script,
  });

  return {
    threadToken: {
      type: "PlutusV2",
      script: threadToken,
    },
    threadTokenPolicyId,

    machineStateAddress,
  };
};

//////////////////////////////////////////////////////////////
const utxos = await lucid?.wallet.getUtxos()!;

const utxo = utxos[0];

const outRef = new Constr(0, [
  new Constr(0, [utxo.txHash]),
  BigInt(utxo.outputIndex),
]);

const tokenName = "threadtoken";

const validatorParametrized = validatorWithParams(tokenName, outRef);

const mintRedeemer = Data.to(new Constr(0, []));

const policyId = validatorParametrized.threadTokenPolicyId;

const assetName = `${policyId}${fromText(tokenName)}`;

console.log("policyId:" + policyId);
console.log("policyId:" + assetName);

const datum = Data.to(
  new Constr(0, [
    BigInt(0),
    "424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b",
  ]),
);

const tx = await lucid
  .newTx()
  .collectFrom([utxo])
  .attachMintingPolicy(validatorParametrized.threadToken as SpendingValidator)
  .mintAssets(
    { [assetName]: BigInt(1) },
    mintRedeemer,
  )
  .payToContract(validatorParametrized.machineStateAddress, { inline: datum }, {
    [assetName]: BigInt(1),
    lovelace: BigInt(20000000),
  })
  .complete();

const signedTx = await tx.sign().complete();

const txHash = await signedTx.submit();

console.log(txHash);
