import DB from "../db";
import API from "../api";
import { BadRequestError } from "../errors";
import { Request, Response } from "express";
import { getSlotId } from "../utils/nano";
import { sellerMiddleware } from "../utils/seller";
import { requireAuth } from "../utils/required";
import { _ } from "../utils/pino";

const createSlotMiddlewares: any = [sellerMiddleware, requireAuth];

/**HANDLER:  creates a contract instance and returns an unbalanced transaction.*/
const createSlotHandler = async (req: Request, res: Response) => {
  const params = req.body;

  const SELLER = req.sellerData;

  let connection: any = null;

  try {
    connection = await DB.client.getConnection();

    await connection.beginTransaction();

    const [product] = await connection.execute(
      "SELECT * FROM product WHERE product_id = ? AND seller_id = ?",
      [params.product_id, SELLER.seller_id]
    );

    if (product.length === 0) {
      throw new Error("NON_EXIST");
    }

    const productData = product[0];

    if (productData.stock < 1) {
      throw new Error("NON_STOCK");
    }

    if (params.quantity > productData.stock) {
      throw new Error("NON_STOCK");
    }

    const scheme = {
      caID: "SlaveContract",
      caWallet: {
        getWalletId: "c08b3754a3fc2c4cb063e12295e903d14edc899d",
      },
    };

    let contractInstances: string[] = [];

    for (let i = 0; i < params.quantity; i++) {
      const response = await API.post("/api/contract/activate", scheme);

      if (!response.data.hasOwnProperty("unContractInstanceId")) {
        throw new Error("failed");
      }

      contractInstances.push(response.data.unContractInstanceId);
    }

    const schemeData = `
    INSERT INTO slot (
      slot_id,
      seller_id,
      product_id,
      instance_id,
      wallet_id,
      schema_v
     ) VALUES (?, ?, ?, ?, ?, ?)`;

    contractInstances.forEach(async (cid) => {
      const schemeValue = [
        "S" + getSlotId(),
        SELLER.seller_id,
        productData.product_id,
        cid,
        params.wallet_id,
        0,
      ];

      console.log(schemeValue);

      await connection.execute(schemeData, schemeValue);
    });

    await connection.commit();

    res.status(200).send({ success: true });
  } catch (err) {
    await connection.rollback();

    _.error(err);

    throw new BadRequestError("failed");
  } finally {
    connection.release();
  }
};

export { createSlotMiddlewares, createSlotHandler };
