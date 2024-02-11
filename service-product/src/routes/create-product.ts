import { BadRequestError } from "../errors";
import { Request, Response } from "express";
import { getProductId } from "../utils/nano";
import { sellerMiddleware } from "../utils/seller";
import { requireAuth } from "../utils/required";
import { getStockStatus } from "../utils/other";
import { _ } from "../utils/pino";
import DB from "../db";
import { clients } from "./get-events";

const createProductMiddlewares: any = [sellerMiddleware, requireAuth];

const createProductHandler = async (req: Request, res: Response) => {
  const params = req.body;

  const SELLER = req.sellerData;

  let connection = null;

  try {
    connection = await DB.client.getConnection();

    await connection.beginTransaction();

    const schemeData = `
    INSERT INTO product (
      product_id,
      seller_id,
      name,
      description,
      category,
      price,
      collateral,
      stock,
      stock_status,
      keywords,
      country,
      image_base,
      image_path,
      image_main,
      image_set,
      schema_v
     ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`;

    const schemeValue = [
      "P" + getProductId(),
      SELLER.seller_id,
      params.name,
      params.description,
      params.category,
      params.price,
      params.collateral,
      params.stock,
      getStockStatus(params.stock),
      params.keywords,
      SELLER.country,
      "http://what",
      "path/image",
      "image1",
      params.image_set,
      0,
    ];

    await connection.execute(schemeData, schemeValue);

    await connection.commit();

    if (clients.hasOwnProperty(SELLER.seller_id)) {
      clients[SELLER.seller_id].write(
        `event:product-created;client:${SELLER.seller_id};`
      );
    }

    res.status(200).send({ success: true });
  } catch (err) {
    await connection.rollback();

    _.error(err);

    throw new BadRequestError("failed");
  } finally {
    connection.release();
  }
};

export { createProductMiddlewares, createProductHandler };
