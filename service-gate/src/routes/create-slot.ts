import DB from "../db";
import API from "../api";
import assert from "assert";
import { BadRequestError } from "../errors";
import { Request, Response } from "express";
import { getSlotId } from "../utils/nano";
import { sellerMiddleware } from "../utils/seller";
import { requireAuth } from "../utils/required";
import { getContractCollateral, getContractPrice } from "../utils/other";
import { _ } from "../utils/pino";

///////////////////////////////////////////////TYPES

interface CreateScheme {
  mode: string;
  iterations: number;
  iteration_units: number;
  iteration_price: number;
  iteration_collateral: number;
  product_discount: number;
}

interface InstanceScheme {
  caID: string;
  caWallet: {
    getWalletId: string;
  };
}

///////////////////////////////////////////////MIDDLEWARES

const createSlotMiddlewares: any = [sellerMiddleware, requireAuth];

///////////////////////////////////////////////

/**HANDLER: create product slots*/
const createSlotHandler = async (req: Request, res: Response) => {
  let connection: any = null;

  const params = req.body;

  const SELLER = req.sellerData;

  const instanceScheme: InstanceScheme = {
    caID: "SlaveContract",
    caWallet: {
      getWalletId: params.wallet_id,
    },
  };

  let scheme: CreateScheme = {
    mode: "",
    iterations: 0,
    iteration_units: 0,
    iteration_price: 0,
    iteration_collateral: 0,
    product_discount: 0,
  };

  try {
    connection = await DB.client.getConnection();

    await connection.beginTransaction();

    const [products] = await connection.execute(
      "SELECT * FROM products WHERE id = ? AND seller_id = ?",
      [params.product_id, SELLER.id]
    );

    if (products.length === 0) {
      throw new BadRequestError("NO_PRODUCT");
    }

    const PRODUCT = products[0];

    ///////////////////////////////

    if (params.batch_mode === true) {
      scheme.mode = "batch";
      scheme.iterations = params.batch_number;
      scheme.iteration_units = params.product_units;
      scheme.iteration_price = getContractPrice(
        "batch",
        PRODUCT.price,
        params.product_discount,
        params.product_units
      );
      scheme.iteration_collateral = getContractCollateral(
        "batch",
        PRODUCT.collateral,
        params.product_units
      );
      scheme.product_discount = params.product_discount;
    }

    if (params.batch_mode === false) {
      scheme.mode = "unit";
      scheme.iterations = params.product_units;
      scheme.iteration_units = 1;
      scheme.iteration_price = getContractPrice(
        "unit",
        PRODUCT.price,
        params.product_discount,
        params.product_units
      );
      scheme.iteration_collateral = PRODUCT.collateral;
      scheme.product_discount = 0;
    }

    ///////////////////////////////

    const schemeData = `
    INSERT INTO slots (
      id,
      mode,
      seller_id,
      contract_id,
      contract_wid,
      contract_units,
      contract_price,
      contract_collateral,
      product_id,
      product_price,
      product_collateral,
      product_discount,
      schema_v
     ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`;

    console.log(scheme);

    for (let i = 0; i < scheme.iterations; i++) {
      try {
        console.log(i);

        const contractInstance = await API.post(
          "/api/contract/activate",
          instanceScheme
        ).then((res) => {
          assert.ok(res.data.hasOwnProperty("unContractInstanceId"));
          return res.data.unContractInstanceId;
        });

        const schemeValue = [
          "S" + getSlotId(),
          scheme.mode,
          SELLER.id,
          contractInstance,
          params.wallet_id,
          scheme.iteration_units,
          scheme.iteration_price,
          scheme.iteration_collateral,
          PRODUCT.id,
          PRODUCT.price,
          PRODUCT.collateral,
          scheme.product_discount,
          0,
        ];

        await connection.execute(schemeData, schemeValue);
      } catch (err) {
        throw new BadRequestError("CID_FAILED");
      }
    }

    await connection.commit();

    res.status(200).send({ success: true });
  } catch (err: any) {
    await connection.rollback();
  } finally {
    connection.release();
  }
};

export { createSlotMiddlewares, createSlotHandler };
