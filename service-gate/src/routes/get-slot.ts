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
      SELECT * FROM slots WHERE id = ?     
      `,
      [params.id]
    );

    if (slots.length === 0) {
      throw new Error("NOT_SLOT");
    }

    await connection.commit();

    res.status(200).send({ success: true, payload: slots });
  } catch (err: any) {
    await connection.rollback();

    throw new BadRequestError(err.message);
  } finally {
    connection.release();
  }
};

export { getSlotMiddlewares, getSlotHandler };
