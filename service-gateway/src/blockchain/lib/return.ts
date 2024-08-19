import {
    applyDoubleCborEncoding,
    applyParamsToScript,
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
} from "https://deno.land/x/lucid@0.10.7/src/mod.ts";
import * as cbor from "https://deno.land/x/cbor@v1.4.1/index.js";

import blueprint from "./plutus.json" assert { type: "json" };

const lucid = await Lucid.new(
    new Blockfrost(
        "https://cardano-preprod.blockfrost.io/api/v0",
        "preprodex26NYImZOT84XAA67qhyHyA7TT6PCGI",
    ),
    "Preprod",
);

const externalWallet =
    "addr_test1qppygdhzm0t7nnlclmds3dy0wc3du870dpy48juu0xxuu2aefdfvc4e0785y7vfhwlmsn3rn26mzvv9md0mhnkpjlc4s0jshh4";

const externalUtxos = await lucid.utxosAt(externalWallet);

lucid.selectWalletFrom({ address: externalWallet, utxos: externalUtxos });

export type Validators = {
    stateMachine: SpendingValidator;
    threadToken: MintingPolicy;
};

export function readValidators(): Validators {
    const threadToken = blueprint.validators.find((v) =>
        v.title === "marketplace.threadtoken"
    );

    if (!threadToken) {
        throw new Error("Redeem validator not found");
    }

    const stateMachine = blueprint.validators.find(
        (v: any) => v.title === "marketplace.statemachine",
    );

    if (!stateMachine) {
        throw new Error("Gift Card validator not found");
    }

    return {
        threadToken: {
            type: "PlutusV2",
            script: threadToken.compiledCode,
        },
        stateMachine: {
            type: "PlutusV2",
            script: stateMachine.compiledCode,
        },
    };
}

const validators = await readValidators();

export function applyParams(
    policyId: string,
) {
    const stateMachine = applyParamsToScript(validators.stateMachine.script, [
        policyId,
    ]);

    return {
        stateMachine: {
            type: "PlutusV2",
            script: applyDoubleCborEncoding(stateMachine),
        },
    };
}

const stateMachineAddress =
    "addr_test1wz7uw5h663gf300s3g69cupf85cdxwshca4rz3835vtnarcl52kpy";

const scriptUtxos = await lucid.utxosAt(stateMachineAddress);

const policyId = "69a2e84c66142e153f255d98786114fbda6d5b5e4b9b1eaa352d88e0";

const parametrizedValidator = applyParams(policyId);

const Datum = Data.Object({
    lock_until: Data.BigInt,
    owner: Data.String,
    beneficiary: Data.String,
});

type Datum = Data.Static<typeof Datum>;

const currentTime = new Date().getTime();

const utxos = scriptUtxos.filter((utxo) => {
    let datum = Data.from<Datum>(
        utxo.datum,
        Datum,
    );

    return datum.beneficiary === beneficiaryPublicKeyHash &&
        datum.lock_until <= currentTime;
});

const laterTime = new Date(currentTime + 2 * 60 * 60 * 1000).getTime(); // add two hours (TTL: time to live)

const stateMachineRedeemer = Data.to(new Constr(3, []));

const tx = await lucid
    .newTx()
    .collectFrom(utxos, stateMachineRedeemer)
    .addSignerKey("424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b")
    .validFrom(currentTime)
    .validTo(laterTime)
    .attachSpendingValidator(
        parametrizedValidator.stateMachine as SpendingValidator,
    )
    .complete();

const transaction = await tx.toString();

console.log(transaction);






import { Core } from '@blaze-cardano/sdk';

function toUnixTime(config: Core.SlotConfig, slot: Core.Slot): UnixTime {
  const { zeroSlot, slotLength, zeroTime } = config;
  const deltaSlot = slot - zeroSlot;
  const halfSlotLength = Math.floor(0.5 * slotLength);
  const msAfterZeroSlot = deltaSlot * slotLength + halfSlotLength;
  return zeroTime + msAfterZeroSlot;
}

function toSlot(config: Core.SlotConfig, unixTime: number): Core.Slot {
  const { zeroSlot, slotLength, zeroTime } = config;
  const timePassed = unixTime - zeroTime;
  const slotsPassed = Math.floor(timePassed / slotLength);
  return Core.Slot(zeroSlot + slotsPassed);
}

const mySlot = toSlot(Core.SLOT_CONFIG_NETWORK.Mainnet, Date.now())

