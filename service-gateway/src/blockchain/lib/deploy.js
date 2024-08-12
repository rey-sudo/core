import {
  ColdWallet,
  Core,
  Data,
  Blaze,
  makeValue,
  Blockfrost,
  MintingPolicy
} from "@blaze-cardano/sdk";

const provider = new Blockfrost({
  network: "cardano-preprod",
  projectId: "preprodex26NYImZOT84XAA67qhyHyA7TT6PCGI",
});

const externalWallet = Core.addressFromBech32(
  "addr_test1qppygdhzm0t7nnlclmds3dy0wc3du870dpy48juu0xxuu2aefdfvc4e0785y7vfhwlmsn3rn26mzvv9md0mhnkpjlc4s0jshh4"
);

const wallet = new ColdWallet(externalWallet, 2, provider);

const blaze = await Blaze.from(provider, wallet);


///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
async function readBlueprint() {
  try {
    const data = await fs.readFile('plutus.json', 'utf8');
    console.log(data);
  } catch (err) {
    console.error(err);
  }
}

const blueprint = readBlueprint();


const readValidators = () => {
  const threadToken = blueprint.validators.find(
    (v) => v.title === "marketplace.threadtoken",
  );

  if (!threadToken) {
    throw new Error("threadToken validator not found");
  }

  const stateMachine = blueprint.validators.find((v) =>
    v.title === "marketplace.statemachine"
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

const validatorsWithParams = (tokenName, outRef) => {
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

if (localWalletUtxos.length < 1) {
  throw new Error("FEE_WALLET_NO_UTXOS");
}

const utxo = localWalletUtxos[0];

const outRef = new Constr(0, [
  new Constr(0, [utxo.txHash]),
  BigInt(utxo.outputIndex),
]);

const tokenName = "threadtoken";

const parameterizedValidators = validatorsWithParams(tokenName, outRef);

const policyId = parameterizedValidators.threadTokenPolicyId;

const stateMachineUnit: Unit = policyId + fromText(tokenName);

const productCollateral = 25n * 1_000_000n;

const stateMachineDatum = Data.to(
  new Constr(0, [
    BigInt(0),
    "424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b",
    productCollateral,
  ]),
);

const minUtxoLovelace = 2n * 1_000_000n;

const threadTokenInput = Data.to(new Constr(0, []));

const localWallet = await lucid.wallet.address();

const tx = await lucid
  .newTx()
  .collectFrom([utxo])
  .attachMintingPolicy(parameterizedValidators.threadToken as MintingPolicy)
  .mintAssets(
    { [stateMachineUnit]: BigInt(1) },
    threadTokenInput,
  )
  .payToContract(
    parameterizedValidators.stateMachineAddress,
    { inline: stateMachineDatum },
    {
      [stateMachineUnit]: BigInt(1),
      lovelace: BigInt(minUtxoLovelace),
    },
  )
  .complete({
    change: {
      address: localWallet,
    },
  });

const signedTx = await tx.sign().complete();

const txHash = await signedTx.submit();

console.log("tx: " + txHash);

console.log("policyId: " + policyId);

console.log("threadTokenUnit: " + stateMachineUnit);

console.log(
  "stateMachineAddress: " + parameterizedValidators.stateMachineAddress,
);

console.log(
  "stateMachineScript: ",
  parameterizedValidators.stateMachine.script,
);
