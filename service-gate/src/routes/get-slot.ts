import DB from "../db";
import { BadRequestError, NotAuthorizedError } from "../errors";
import { Request, Response } from "express";
import { sellerMiddleware } from "../utils/seller";
import { userMiddleware } from "../utils/user";

const getSlotMiddlewares: any = [sellerMiddleware, userMiddleware];

const getSlotHandler = async (req: Request, res: Response) => {
  const params = req.params;

  const SELLER = req.sellerData;
  const BUYER = req.userData;

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
      slots.buyer_pubkeyhash,
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
          'model', products.model,
          'product_price', products.price,
          'media_url', products.media_url,
          'media_path', products.media_path,
          'image_main', products.image_main
      ) AS product_details

      FROM slots
      JOIN products ON slots.product_id = products.id
      WHERE slots.id = ?;
      `,
      [params.id]
    );

    await connection.commit();

    if (slots.length === 0) {
      throw new BadRequestError("NOT_SLOT");
    }

    const SLOT = slots[0];

    console.log(SLOT);

    if (SLOT.seller_id === SELLER?.id) {
      return res.status(200).send({ success: true, payload: SLOT });
    }

    if (SLOT.buyer_pubkeyhash === BUYER?.pubkeyhash) {
      return res.status(200).send({ success: true, payload: SLOT });
    }

    throw new NotAuthorizedError();
  } catch (err: any) {
    await connection.rollback();

    res.status(404).send({ success: false });
  } finally {
    connection.release();
  }
};

export { getSlotMiddlewares, getSlotHandler };
