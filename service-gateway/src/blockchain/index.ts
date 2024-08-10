import {
    applyParamsToScript,
    Data,
    fromText,
    Lucid,
    MintingPolicy,
    SpendingValidator,
} from "lucid-cardano";
import { readFile } from "fs/promises";

const getLucid: any = async () => await Lucid.new();

const lucid: Lucid = getLucid();

const loadBlueprint = async () => {
    try {
        const data = await readFile("./plutus.json", "utf8");

        return JSON.parse(data);
    } catch (err) {
        console.error("Error reading JSON:", err);
    }
};

const blueprint: any = loadBlueprint();

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

const validators = readValidators();

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

export { validators, validatorsWithParams };
