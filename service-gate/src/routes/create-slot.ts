import DB from "../db";
import API from "../api";
import assert from "assert";
import { BadRequestError } from "../errors";
import { Request, Response } from "express";
import { getSlotId } from "../utils/nano";
import { sellerMiddleware } from "../utils/seller";
import { requireAuth } from "../utils/required";
import { _ } from "../utils/pino";

const createSlotMiddlewares: any = [sellerMiddleware, requireAuth];

/**HANDLER:  creates a contract instance*/
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
  try {
    connection = await DB.client.getConnection();

    await connection.beginTransaction();

    const [product] = await connection.execute(
      "SELECT * FROM product WHERE product_id = ? AND seller_id = ?",
      [params.product_id, SELLER.seller_id]
    );

    if (product.length === 0) {
      throw new Error("NOT_PRODUCT");
    }

    const productData = product[0];

    if (productData.stock < 1) {
      throw new Error("NOT_STOCK");
    }

    const contractInstance = await API.post("/api/contract/activate", cidScheme)
      .then((res) => {
        assert.ok(res.data.hasOwnProperty("unContractInstanceId"));
        return res.data.unContractInstanceId;
      })
      .catch(() => {
        throw new Error("CID_FAILED");
      });

    const schemeData = `
    INSERT INTO slot (
      slot_id,
      wallet_id,
      instance_id,
      seller_id,
      product_id,
      product_price,
      product_collateral,
      schema_v
     ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)`;

    const schemeValue = [
      "S" + getSlotId(),
      params.wallet_id,
      contractInstance,
      SELLER.seller_id,
      productData.product_id,
      productData.price,
      productData.collateral,
      0,
    ];

    console.log(schemeValue);

    await connection.execute(schemeData, schemeValue);

    await connection.commit();

    res.status(200).send({ success: true });
  } catch (err: any) {
    await connection.rollback();

    throw new BadRequestError(err.message);
  } finally {
    connection.release();
  }
};

export { createSlotMiddlewares, createSlotHandler };
