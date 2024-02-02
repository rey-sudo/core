import DB from "../db";
import API from "../api";
import { BadRequestError } from "../errors";
import { Request, Response } from "express";
import { getSlotId } from "../utils/nano";
import { sellerMiddleware } from "../utils/seller";
import { requireAuth } from "../utils/required";
import { _ } from "../utils/pino";

const createSlotMiddlewares: any = [sellerMiddleware, requireAuth];

const createSlotHandler = async (req: Request, res: Response) => {
  const params = req.body;

  const seller = req.sellerData;

  let connection = null;

  const scheme = {
    caID: "SlaveContract",
    caWallet: {
      getWalletId: "c08b3754a3fc2c4cb063e12295e903d14edc899d",
    },
  };

  await API.post("/api/contract/activate", scheme)
    .then((res) => {
      console.log(res);
    })
    .catch((err) => console.error(err));
  
  return res.status(200).send({ success: true });


  try {
    connection = await DB.client.getConnection();

    await connection.beginTransaction();

    //check product

    //check slots

    const schemeData = `
    INSERT INTO slot (
      order_id,
      seller_id,
      product_id,
      instance_id,
      wallet_id,
      schema_v
     ) VALUES (?, ?, ?, ?, ?, ?)`;

    const schemeValue = [
      getSlotId(),
      seller.seller_id,
      params.product_id,
      params.instance_id,
      params.wallet_id,
      0,
    ];

    await connection.execute(schemeData, schemeValue);

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
