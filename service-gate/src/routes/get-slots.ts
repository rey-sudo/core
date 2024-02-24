import DB from "../db";
import { BadRequestError } from "../errors";
import { Request, Response } from "express";
import { sellerMiddleware } from "../utils/seller";
import { requireAuth } from "../utils/required";

const getSlotsMiddlewares: any = [sellerMiddleware, requireAuth];

const getSlotsHandler = async (req: Request, res: Response) => {
  const params = req.body;

  const SELLER = req.sellerData;

  let connection = null;

  try {
    connection = await DB.client.getConnection();

    const [response] = await connection.execute(
      `SELECT *
       FROM product
       JOIN slot ON product.product_id = slot.product_id
       WHERE product.seller_id = ?;
      `,
      [SELLER.seller_id]
    );

    await connection.commit();

    res.status(200).send({ success: true, payload: response });
  } catch (err: any) {
    await connection.rollback();

    throw new BadRequestError(err.message);
  } finally {
    connection.release();
  }
};

export { getSlotsMiddlewares, getSlotsHandler };
