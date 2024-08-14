import {
    Blockfrost,
    Core
} from "@blaze-cardano/sdk";
import { MarketplaceStatemachine, MarketplaceThreadtoken } from "./plutus";

const provider = new Blockfrost({
    network: "cardano-preprod",
    projectId: "preprodex26NYImZOT84XAA67qhyHyA7TT6PCGI",
});

const validatorsWithParams = (tokenName: any, utxoRef: any) => {
    const threadTokenScript = new MarketplaceThreadtoken(tokenName, utxoRef);

    const threadTokenPolicyId = threadTokenScript.hash();

    const stateMachineScript = new MarketplaceStatemachine(threadTokenPolicyId);

    const stateMachineAddress = Core.addressFromValidator(
        Core.NetworkId.Testnet,
        stateMachineScript,
    );

    return {
        threadTokenScript,
        stateMachineScript,
        threadTokenPolicyId,
        stateMachineAddress,
    };
};
export { provider, validatorsWithParams };
