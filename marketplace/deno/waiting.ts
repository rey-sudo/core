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

const policyId = "3681dd85bf4383dcf86b661fa991d7db6013cc7ad2477d094d799a73";

const utxo: UTxO = {
  txHash: "18d2881f528359a953b2149db71f48d2b9b554c53e00039f459a712b475934d2",
  outputIndex: 0,
  assets: {
    lovelace: 1236970n,
    "3681dd85bf4383dcf86b661fa991d7db6013cc7ad2477d094d799a73746872656164746f6b656e":
      1n,
  },
  address: "addr_test1wr3x8r2g50dn55y28fvxr4na0h0g7d6y42mzlh8krqnuttqakfhve",
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
    BigInt(0),
    "424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b",
  ]),
);

const tx = await lucid
  .newTx()
  .collectFrom([utxo], redeemer)
  .payToContract(validatorParametrized.machineStateAddress, { inline: datum }, {
    lovelace: 10000000n,
    [assetName]: BigInt(1),
  })
  .attachSpendingValidator(validators.machineState as SpendingValidator)
  .complete();

console.log(await tx.toString());
