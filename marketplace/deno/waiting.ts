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
  UTxO,
} from "https://deno.land/x/lucid@0.10.7/mod.ts";
import * as cbor from "https://deno.land/x/cbor@v1.4.1/index.js";

const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preview.blockfrost.io/api/v0",
    "previewgTjbjYtdKdOcNmhtu6H9snNl3DhnaxQf",
  ),
  "Preview",
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

const validators = await readValidators();

const validatorWithParams = () => {
  const machineStateAddress = lucid.utils.validatorToAddress({
    type: "PlutusV2",
    script: validators.machineState.script,
  });

  return {
    machineStateAddress,
  };
};

//////////////////////////////////////////////////////////////
lucid.selectWalletFromPrivateKey(await Deno.readTextFile("./me.sk"));

const policyId = "98c346b491ddeea02185a5ecc3349009e179a9042947c049a2c90938";

const utxo: UTxO = {
  txHash: "d51227e035e83f6be1bfa250d7eca3de58696b6a8ed33a9dc6088974d5419020",
  outputIndex: 0,
  assets: {
    lovelace: 1236970n,
    "98c346b491ddeea02185a5ecc3349009e179a9042947c049a2c90938746872656164746f6b656e":
      1n,
  },
  address: "addr_test1wp4ep7h3mw4fvse8v8lmafzjpettgfm972r783mzlcemzrg5avvkf",
  datumHash: "1cc953c6981e5e524f90f459f28847ab24455c9ee3ae7c8916d4889ceb2d8a11",
  datum:
    "d8799f00581c424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bff",
  scriptRef: null,
};

const tokenName = "threadtoken";

const validatorParametrized = validatorWithParams();

const assetName = `${policyId}${fromText(tokenName)}`;

const redeemer = Data.to(new Constr(0, []));

const datum = Data.to(
  new Constr(0, [
    BigInt(1),
    "424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b",
  ]),
);

try {
  const tx = await lucid
    .newTx()
    .collectFrom([utxo], redeemer)
    .addSignerKey("424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b")
    .payToContract(
      validatorParametrized.machineStateAddress,
      { inline: datum },
      {
        lovelace: BigInt(10000000),
      },
    )
    .payToAddress(validatorParametrized.machineStateAddress, {
      [assetName]: BigInt(1),
    })
    .attachSpendingValidator(validators.machineState as SpendingValidator)
    .complete();

  console.log(await tx.toString());
} catch (err) {
  console.log(err);
}
