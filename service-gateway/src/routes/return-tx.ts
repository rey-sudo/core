import DB from "../db";
import { Request, Response } from "express";
import { userMiddleware } from "../utils/user";

const returnTxMiddlewares: any = [userMiddleware];

////////////////////////////////////////////////////

const returnTxHandler = async (req: Request, res: Response) => {
  let connection: any = null;

  const params = req.body;

  const BUYER = req.userData;

  try {
    connection = await DB.client.getConnection();

    await connection.beginTransaction();

    //////////////////////////////////////////////

    const schemeData = `
      UPDATE orders 
      SET status = ?,
          buyer_pubkeyhash = ?,
          contract_return_tx = ?
      WHERE id = ?
      `;

    const schemeValue = [
      "returned",
      BUYER.pubkeyhash,
      params.tx_hash,
      params.order_id
    ];

    await connection.execute(schemeData, schemeValue);

    await connection.commit();

    res.status(200).send({
      success: true,
    });
  } catch (err: any) {
    await connection.rollback();

    res.status(404).send({
      success: true,
    });
  } finally {
    connection.release();
  }
};

export { returnTxHandler, returnTxMiddlewares };
