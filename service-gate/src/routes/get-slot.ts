import DB from "../db";
import { BadRequestError } from "../errors";
import { Request, Response } from "express";
import { sellerMiddleware } from "../utils/seller";
import { requireAuth } from "../utils/required";

const getSlotMiddlewares: any = [sellerMiddleware, requireAuth];

const getSlotHandler = async (req: Request, res: Response) => {
  const params = req.params;

  const SELLER = req.sellerData;

  let connection = null;

  try {
    connection = await DB.client.getConnection();

    const [slots] = await connection.execute(
      `
      SELECT 
      slots.id,
      slots.mode,
      slots.status,
      slots.actived,
      slots.seller_id,
      slots.contract_stage,
      slots.contract_units, 
      slots.contract_price,
      slots.contract_collateral,
      slots.contract_0_utx,
      slots.contract_0_tx,
      slots.contract_1_tx,
      slots.contract_2_tx,
      slots.contract_3_tx,
      slots.created_at,
      JSON_OBJECT(
          'product_id', products.id,
          'product_name', products.name,
          'product_price', products.price
      ) AS product_details

      FROM slots
      JOIN products ON slots.product_id = products.id
      WHERE slots.id = ?;
      `,
      [params.id]
    );

    console.log(slots);

    if (slots.length === 0) {
      throw new BadRequestError("NOT_SLOT");
    }

    await connection.commit();

    res.status(200).send({ success: true, payload: slots[0] });
  } catch (err: any) {
    await connection.rollback();
  } finally {
    connection.release();
  }
};

export { getSlotMiddlewares, getSlotHandler };
