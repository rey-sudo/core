import DB from "../db";
import { Blaze, ColdWallet, Core, Data, makeValue } from "@blaze-cardano/sdk";
import { Request, Response } from "express";
import { userMiddleware } from "../utils/user";
import { provider } from "../blockchain";
import { BadRequestError } from "../errors";
import { MarketplaceStatemachine } from "../blockchain/plutus";
import { redisDB } from "../db/redis";
import { _ } from "../utils/pino";

const lockingMiddlewares: any = [userMiddleware];

const lockingHandler = async (req: Request, res: Response) => {
  const params = req.body;

  const BUYER = req.userData;

  let connection: any = null;

  try {
    connection = await DB.client.getConnection();

    const [orders] = await connection.execute(
      "SELECT * FROM orders WHERE id = ?",
      [params.order_id],
    );

    if (orders.length === 0) {
      throw new Error("NO_ORDER");
    }

    const ORDER = orders[0];

    const isLocked = await redisDB.client.get(ORDER.id);

    console.log(isLocked);

    if (isLocked === "locked") {
      throw new BadRequestError("LOCKED");
    }

    const externalWallet = Core.addressFromBech32(
      BUYER.address,
    );

    const stateMachineAddress = Core.addressFromBech32(
      ORDER.contract_address,
    );

    const wallet = new ColdWallet(
      externalWallet,
      Core.NetworkId.Testnet,
      provider,
    );

    const blaze = await Blaze.from(provider, wallet);

    const threadTokenUnit = ORDER.contract_unit;

    const assetId = Core.AssetId(threadTokenUnit);

    const threadTokenPolicyId = Core.AssetId.getPolicyId(assetId);

    const threadTokenUtxos = await provider.getUnspentOutputsWithAsset(
      stateMachineAddress,
      assetId,
    );

    if (threadTokenUtxos.length < 1) {
      throw new BadRequestError("NO_THREADTOKEN_UTXO");
    }

    if (threadTokenUtxos.length > 1) {
      throw new BadRequestError("THREADTOKEN_QUANTITY");
    }

    const stateMachineInput = Data.Enum([
      Data.Literal("Cancel"),
      Data.Object({ Locking: Data.Object({ buyer_param: Data.Bytes() }) }),
      Data.Literal("Shipping"),
      Data.Literal("Received"),
    ]);

    const lockingInput = {
      Locking: {
        buyer_param: BUYER.pubkeyhash,
      },
    };

    const stateMachineRedeemer = Data.to(lockingInput, stateMachineInput);

    const stateMachineScript = new MarketplaceStatemachine(threadTokenPolicyId);

    const productPrice = BigInt(ORDER.contract_price);

    const productCollateral = BigInt(ORDER.contract_collateral);

    const threadTokenAsset = makeValue(productPrice + productCollateral, [
      threadTokenUnit,
      1n,
    ]);

    const minFee = 1n * 1_000_000n;

    const data = {
      state: 1n,
      seller: ORDER.seller_pubkeyhash,
      collateral: productCollateral,
      price: productPrice,
      buyer: BUYER.pubkeyhash,
    };

    const Datum = Data.Object({
      state: Data.Integer(),
      seller: Data.Bytes(),
      collateral: Data.Integer(),
      price: Data.Integer(),
      buyer: Data.Nullable(Data.Bytes()),
    });

    const lockingDatum = Data.to(data, Datum);

    const tx = await blaze
      .newTransaction()
      .addInput(threadTokenUtxos[0], stateMachineRedeemer)
      .lockAssets(stateMachineAddress, threadTokenAsset, lockingDatum)
      .provideScript(stateMachineScript)
      .addRequiredSigner(Core.Ed25519KeyHashHex(BUYER.pubkeyhash))
      .setChangeAddress(externalWallet)
      .setMinimumFee(minFee)
      .complete();

    const transaction = tx.toCbor();

    console.log(transaction);

    await redisDB.client.set(ORDER.id, "locked", {
      EX: 10,
      NX: true,
    });

    //////////////////////////////////////////////

    await connection.commit();

    res.status(200).send({
      success: true,
      payload: {
        transaction,
      },
    });
  } catch (err: any) {
    await connection.rollback();

    _.error(err);

    res.status(404).send({
      success: false,
    });
  } finally {
    connection.release();
  }
};

export { lockingHandler, lockingMiddlewares };
