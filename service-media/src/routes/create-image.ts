import DB from "../db";
import API from "../api";
import { BadRequestError } from "../errors";
import { Request, Response } from "express";
import { getSlotId } from "../utils/nano";
import { sellerMiddleware } from "../utils/seller";
import { requireAuth } from "../utils/required";
import { _ } from "../utils/pino";

const createImageMiddlewares: any = [sellerMiddleware, requireAuth];

const createImageHandler = async (req: Request, res: Response) => {
  const params = req.body;

  const sellerDatum = req.sellerData;

  let connection: any = null;

  try {
    connection = await DB.client.getConnection();

    await connection.beginTransaction();

    const [product] = await connection.execute(
      "SELECT * FROM product WHERE product_id = ?",
      [params.product_id]
    );

    if (product.length === 0) {
      throw new Error("nonexist");
    }

    const productData = product[0];

    if (productData.slots === 0) {
      throw new Error("nonslots");
    }

    if (params.quantity > productData.slots) {
      throw new Error("nonslots");
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
        getSlotId(),
        sellerDatum.seller_id,
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

export { createImageMiddlewares, createImageHandler };
