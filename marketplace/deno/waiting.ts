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

const policyId = "54a29c2626156de3af97cdead84264aaf0805857cc5c026af077fc3b";

const utxo: UTxO = {
  txHash: "919818c757b8242ffc60ed2ca4bdf4d2c0dea87203075143b1565978197dac3b",
  outputIndex: 0,
  assets: {
    lovelace: 10000000n,
    "54a29c2626156de3af97cdead84264aaf0805857cc5c026af077fc3b746872656164746f6b656e":
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

const externalAddress =
  "addr_test1qppygdhzm0t7nnlclmds3dy0wc3du870dpy48juu0xxuu2aefdfvc4e0785y7vfhwlmsn3rn26mzvv9md0mhnkpjlc4s0jshh4";

const utxis = await lucid.utxosAt(
  externalAddress,
);

const localChange = await lucid.wallet.address();

const productCollateral = 23000000n;

const externalADA = 9606467635n;
try {
  const tx = await lucid
    .newTx()
    .collectFrom([utxo, utxis[0]], redeemer)
    .addSignerKey("424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b")
    .payToContract(
      validatorParametrized.machineStateAddress,
      { inline: datum },
      {
        [assetName]: BigInt(1),
        lovelace: BigInt(productCollateral),
      },
    )
    .payToAddress(externalAddress, {
      lovelace: BigInt(externalADA - productCollateral),
    })
    .attachSpendingValidator(validators.machineState as SpendingValidator)
    .complete({
      change: {
        address: localChange,
      },
    });

  console.log(await tx.toString());
} catch (err) {
  console.log(err);
}
