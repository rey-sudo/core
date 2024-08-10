import DB from "../db";
import { Request, Response } from "express";
import { requireAuth } from "../utils/required";
import { sellerMiddleware } from "../utils/seller";
import { BadRequestError } from "../errors";
import {
  Blockfrost,
  Constr,
  Data,
  fromText,
  Lucid,
  MintingPolicy,
  Unit,
} from "lucid-cardano";
import { validatorsWithParams } from "../blockchain";

const BLOCKFROST_URL = "https://cardano-preprod.blockfrost.io/api/v0";

const BLOCKFROST_KEY = "preprodex26NYImZOT84XAA67qhyHyA7TT6PCGI";

const BLOCKFROST_ENV = "Preprod";

////////////////////////////////////////////////////

const deployMiddlewares: any = [sellerMiddleware, requireAuth];

////////////////////////////////////////////////////

const deployHandler = async (req: Request, res: Response) => {
  let connection: any = null;

  const params = req.body;

  const SELLER = req.sellerData;

  try {
    connection = await DB.client.getConnection();

    await connection.beginTransaction();

    const [orders] = await connection.execute(
      "SELECT * FROM orders WHERE id = ? AND seller_id = ?",
      [params.order_id, SELLER.id],
    );

    if (orders.length === 0) {
      throw new Error("NO_ORDER");
    }

    const ORDER = orders[0];

    if (ORDER.deployed) {
      throw new Error("IS_DEPLOYED");
    }

    /////////////////////////////////////////////////////////////

    const lucid = await Lucid.new(
      new Blockfrost(
        BLOCKFROST_URL,
        BLOCKFROST_KEY,
      ),
      BLOCKFROST_ENV,
    );

    const LOCAL_SEED =
      "bulk ahead math cloud retreat manual antenna ahead autumn bird element stumble fiction tell magic cross arm payment breeze suggest pig version expand divert";

    lucid.selectWalletFromSeed(LOCAL_SEED);

    console.log(await lucid.wallet.address());

    /////////////////
    
    const localWalletUtxos = await lucid.wallet.getUtxos();

    if (localWalletUtxos.length < 1) {
      throw new BadRequestError("FEE_WALLET_NO_UTXOS");
    }

    const utxo = localWalletUtxos[0];

    const outRef = new Constr(0, [
      new Constr(0, [utxo.txHash]),
      BigInt(utxo.outputIndex),
    ]);

    const tokenName = "threadtoken";

    const parameterizedValidators = validatorsWithParams(tokenName, outRef);

    const threadTokenPolicyId = parameterizedValidators.threadTokenPolicyId;

    const stateMachineUnit: Unit = threadTokenPolicyId + fromText(tokenName);

    const productCollateral = BigInt(ORDER.contract_collateral);

    const stateMachineDatum = Data.to(
      new Constr(0, [
        BigInt(0),
        params.seller_pubkeyhash,
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

    const transaction = await signedTx.submit();

    console.log("tx: " + transaction);

    console.log("threadTokenPolicyId: " + threadTokenPolicyId);

    console.log("threadTokenUnit: " + stateMachineUnit);

    console.log(
      "stateMachineAddress: " + parameterizedValidators.stateMachineAddress,
    );

    console.log(
      "stateMachineScript: ",
      parameterizedValidators.stateMachine.script,
    );

    //////////////////////////////////////////////////////7

    const schemeData = `
      UPDATE orders 
      SET status = ?,
          deployed = ?,
          seller_pubkeyhash = ?,
          contract_address = ?,
          contract_state = ?,
          contract_threadtoken = ?,
          contract_0_tx = ?
      WHERE id = ? AND seller_id = ?
      `;

    const schemeValue = [
      "deployed",
      true,
      params.seller_pubkeyhash,
      parameterizedValidators.stateMachineAddress,
      0,
      threadTokenPolicyId,
      transaction,
      params.order_id,
      SELLER.id,
    ];

    await connection.execute(schemeData, schemeValue);

    await connection.commit();

    res.status(200).send({
      success: true,
    });
  } catch (err: any) {
    await connection.rollback();

    res.status(404).send({
      success: false,
    });
  } finally {
    connection.release();
  }
};

export { deployHandler, deployMiddlewares };
