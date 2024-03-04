import DB from "../db";
import API from "../api";
import assert from "assert";
import { BadRequestError } from "../errors";
import { Request, Response } from "express";
import { getSlotId } from "../utils/nano";
import { sellerMiddleware } from "../utils/seller";
import { requireAuth } from "../utils/required";
import { _ } from "../utils/pino";
import { sendEvent } from "./get-events";

interface slotScheme {
  mode: string;
  iterations: number;
  units: number;
  discount: number;
}

const createSlotMiddlewares: any = [sellerMiddleware, requireAuth];

/**HANDLER: creates a contract instance*/
const createSlotHandler = async (req: Request, res: Response) => {
  const params = req.body;

  const SELLER = req.sellerData;

  const cidScheme = {
    caID: "SlaveContract",
    caWallet: {
      getWalletId: params.wallet_id,
    },
  };

  let connection: any = null;

  let slotScheme: slotScheme = {
    mode: "unit",
    iterations: 0,
    units: 0,
    discount: 0,
  };

  if (params.batch_mode === true) {
    slotScheme.mode = "batch";
    slotScheme.iterations = params.batch_number;
    slotScheme.units = params.unit_number;
    slotScheme.discount = params.product_discount;
  }

  if (params.batch_mode === false) {
    slotScheme.mode = "unit";
    slotScheme.iterations = params.unit_number;
    slotScheme.units = 1;
    slotScheme.discount = 0;
  }

  try {
    connection = await DB.client.getConnection();

    await connection.beginTransaction();

    const [products] = await connection.execute(
      "SELECT * FROM products WHERE id = ? AND seller_id = ?",
      [params.product_id, SELLER.id]
    );

    if (products.length === 0) {
      throw new Error("NOT_PRODUCT");
    }

    const PRODUCT = products[0];

    if (PRODUCT.stock < 1) {
      throw new Error("NOT_STOCK");
    }

    const schemeData = `
    INSERT INTO slots (
      id,
      mode,
      wallet_id,
      contract_id,
      seller_id,
      product_id,
      product_price,
      product_collateral,
      product_discount,
      product_units,
      schema_v
     ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`;

    for (let i = 0; i < slotScheme.iterations; i++) {
      const contract_id = await API.post("/api/contract/activate", cidScheme)
        .then((res) => {
          assert.ok(res.data.hasOwnProperty("unContractInstanceId"));
          return res.data.unContractInstanceId;
        })
        .catch(() => {
          throw new Error("CID_FAILED");
        });

      const schemeValue = [
        "S" + getSlotId(),
        slotScheme.mode,
        params.wallet_id,
        contract_id,
        SELLER.id,
        PRODUCT.id,
        PRODUCT.price,
        PRODUCT.collateral,
        slotScheme.discount,
        slotScheme.units,
        0,
      ];

      console.log(schemeValue);
      await connection.execute(schemeData, schemeValue);
    }

    await connection.commit();

    sendEvent(SELLER.id, "slot:created");

    res.status(200).send({ success: true });
  } catch (err: any) {
    await connection.rollback();

    throw new BadRequestError(err.message);
  } finally {
    connection.release();
  }
};

export { createSlotMiddlewares, createSlotHandler };
