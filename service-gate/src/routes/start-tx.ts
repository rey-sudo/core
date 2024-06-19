import DB from "../db";
import { Request, Response } from "express";
import { requireAuth } from "../utils/required";
import { sellerMiddleware } from "../utils/seller";
import { BadRequestError } from "../errors";

////////////////////////////////////////////////////MIDDLEWARES

const startTxMiddlewares: any = [sellerMiddleware, requireAuth];

////////////////////////////////////////////////////

const startTxHandler = async (req: Request, res: Response) => {
  let connection: any = null;

  const params = req.body;

  const SELLER = req.sellerData;

  try {
    connection = await DB.client.getConnection();

    await connection.beginTransaction();

    //////////////////////////////////////////////

    const schemeData = `
      UPDATE slots 
      SET contract_stage = ?,
          contract_0_tx = ?
      WHERE id = ? AND seller_id = ?
      `;

    const schemeValue = ["waiting", params.tx_hash, params.slot_id, SELLER.id];

    await connection.execute(schemeData, schemeValue);

    await connection.commit();

    res.status(200).send({
      success: true,
    });
  } catch (err: any) {
    await connection.rollback();

    throw new BadRequestError(err.message);
  } finally {
    connection.release();
  }
};

export { startTxMiddlewares, startTxHandler };
