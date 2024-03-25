import DB from "../db";
import API from "../api";
import assert from "assert";
import { BadRequestError } from "../errors";
import { Request, Response } from "express";
import { getSlotId } from "../utils/nano";
import { sellerMiddleware } from "../utils/seller";
import { requireAuth } from "../utils/required";
import { sendEvent } from "./get-events";
import { getContractCollateral, getContractPrice } from "../utils/other";
import { _ } from "../utils/pino";

interface createScheme {
  mode: string;
  iterations: number;
  units: number;
  discount: number;
}

const createSlotMiddlewares: any = [sellerMiddleware, requireAuth];

/**HANDLER: creates slots*/
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

  let createScheme: createScheme = {
    mode: "unit",
    iterations: 0,
    units: 0,
    discount: 0,
  };

  if (params.batch_mode === true) {
    createScheme.mode = "batch";
    createScheme.iterations = params.batch_number;
    createScheme.units = params.unit_number;
    createScheme.discount = params.unit_discount;
  }

  if (params.batch_mode === false) {
    createScheme.mode = "unit";
    createScheme.iterations = params.unit_number;
    createScheme.units = 1;
    createScheme.discount = 0;
  }
  console.log(params);

  console.log(createScheme);

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

    if (PRODUCT.stock < createScheme.units) {
      throw new Error("NOT_STOCK");
    }

    const schemeData = `
    INSERT INTO slots (
      id,
      mode,
      seller_id,
      contract_id,
      contract_wid,
      contract_price,
      contract_collateral,
      product_id,
      product_price,
      product_discount,
      product_units,
      schema_v
     ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`;

    //////////////////////////

    for (let i = 0; i < createScheme.iterations; i++) {
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
        createScheme.mode,
        SELLER.id,
        contract_id,
        params.wallet_id,
        getContractPrice(
          PRODUCT.price,
          createScheme.discount,
          createScheme.units
        ),
        getContractCollateral(PRODUCT.collateral, createScheme.units),
        PRODUCT.id,
        PRODUCT.price,
        createScheme.discount,
        createScheme.units,
        0,
      ];

      console.log(schemeValue);
      await connection.execute(schemeData, schemeValue);
    }

    //////////////////////////

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
