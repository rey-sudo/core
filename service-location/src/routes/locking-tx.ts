import DB from "../db";
import { Request, Response } from "express";
import { userMiddleware } from "../utils/user";


const lockingTxMiddlewares: any = [userMiddleware];

////////////////////////////////////////////////////

const lockingTxHandler = async (req: Request, res: Response) => {
  let connection: any = null;

  const params = req.body;

  const BUYER = req.userData;

  try {
    connection = await DB.client.getConnection();

    await connection.beginTransaction();

    //////////////////////////////////////////////

    const schemeData = `
      UPDATE slots 
      SET contract_stage = ?,
          contract_1_tx = ?
      WHERE id = ? AND buyer_pubkeyhash = ?
      `;

    const schemeValue = ["locking", params.tx_hash, params.slot_id, BUYER.pubkeyhash];

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

export { lockingTxMiddlewares, lockingTxHandler };
