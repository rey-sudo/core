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

const externalAddress =
  "addr_test1qppygdhzm0t7nnlclmds3dy0wc3du870dpy48juu0xxuu2aefdfvc4e0785y7vfhwlmsn3rn26mzvv9md0mhnkpjlc4s0jshh4";

const targetAddress =
  "addr_test1qp539v654clv34y7k6zwrtxzczwvzz0dudmgfy5rt3qvjf2hrg74pzy4umh8udkhshpqmwdzluk6zvr5tcrj8h74re2q2yavu8";

const externalAddressUtxos = await lucid.utxosAt(
  externalAddress,
);

lucid.selectWalletFrom({
  address: externalAddress,
  utxos: externalAddressUtxos,
});

try {
  const tx = await lucid.newTx()
    .payToAddress(targetAddress, { lovelace: 15000000n })
    .complete();

  const txCbor = tx.toString();

  console.log(txCbor);
} catch (err) {
  console.log(err);
}
