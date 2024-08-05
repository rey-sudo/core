import {
  Blockfrost,
  C,
  Constr,
  Data,
  fromHex,
  Lucid,
  MintingPolicy,
  SpendingValidator,
  toHex,
  TxHash,
  utf8ToHex,
} from "https://deno.land/x/lucid@0.8.3/mod.ts";
import * as cbor from "https://deno.land/x/cbor@v1.4.1/index.js";
import blueprint from "~/plutus.json" assert { type: "json" };

const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preview.blockfrost.io/api/v0",
    "previewgTjbjYtdKdOcNmhtu6H9snNl3DhnaxQf",
  ),
  "Preview",
);

lucid.selectWalletFromPrivateKey(await Deno.readTextFile("./me.sk"));

const validator = await readValidators();

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

//////////////////////////////////////////////////////////////////////////////////

const publicKeyHash = lucid.utils.getAddressDetails(
  await lucid.wallet.address(),
).paymentCredential?.hash;

const datum = Data.to(new Constr(0, [publicKeyHash]));

const txHash = await deploy(1000000n, { validator: validator, datum: datum });

await lucid.awaitTx(txHash);

console.log(`1 tADA locked into the contract at:
    Tx ID: ${txHash}
    Datum: ${datum}
`);

// --- Supporting functions

async function deploy(
  lovelace: bigint,
  { validator, datum }: { validator: SpendingValidator; datum: string },
): Promise<TxHash> {
  const contractAddress = lucid.utils.validatorToAddress(validator);

  console.log(contractAddress);

  const tx = await lucid
    .newTx()
    .payToContract(contractAddress, { inline: datum }, { lovelace })
    .complete();

  const signedTx = await tx.sign().complete();

  return signedTx.submit();
}
