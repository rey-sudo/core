import DB from "../db";
import { Request, Response } from "express";

const buyOptionsMiddlewares: any = [];

const buyOptionsHandler = async (req: Request, res: Response) => {
  const params = req.params;

  let connection = null;

  try {
    connection = await DB.client.getConnection();

    const [response] = await connection.execute(
      `
      SELECT
      id,
      mode,
      contract_units,
      contract_price,
      contract_collateral,
      product_discount,
      COUNT(slots.id) AS slots_count
      FROM 
        slots
      WHERE
        product_id = ?
      AND
        contract_stage = ?    
      `,
      [params.id, "waiting"]
    );

    await connection.commit();

    res.status(200).send({ success: true, payload: response });
  } catch (err: any) {
    await connection.rollback();

    res.status(200).send({ success: false });
  } finally {
    connection.release();
  }
};

export { buyOptionsMiddlewares, buyOptionsHandler };
