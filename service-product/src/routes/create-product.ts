import DB from "../db";
import { Request, Response } from "express";
import { getProductId } from "../utils/nano";
import { sellerMiddleware } from "../utils/seller";
import { requireAuth } from "../utils/required";
import { BadRequestError } from "../errors";
import { _ } from "../utils/pino";

const createProductMiddlewares: any = [sellerMiddleware, requireAuth];

const createProductHandler = async (req: Request, res: Response) => {
  const params = req.body;

  const SELLER = req.sellerData;

  let connection = null;

  try {
    if (params.collateral > params.price) {
      throw new BadRequestError("MAX_COLLATERAL");
    }

    connection = await DB.client.getConnection();

    await connection.beginTransaction();

    const schemeData = `
    INSERT INTO products (
      id,
      seller_id,
      name,
      model,
      features,
      terms_of_sale,
      guarantee,
      category,
      price,
      collateral,
      stock,
      keywords,
      country,
      media_url,
      media_path,
      image_main,
      image_set,
      video_set,
      schema_v
     ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`;

    const schemeValue = [
      "P" + getProductId(),
      SELLER.id,
      params.name,
      params.model,
      params.features,
      params.terms_of_sale,
      params.guarantee,
      params.category,
      params.price,
      params.collateral,
      params.stock,
      params.keywords,
      SELLER.country,
      "https://pairfy.dev",
      "/api/media/get-image/",
      params.image_set.split(",")[0],
      params.image_set,
      "",
      0,
    ];

    await connection.execute(schemeData, schemeValue);

    await connection.commit();

    res.status(200).send({ success: true });
  } catch (err) {
    await connection.rollback();

    _.error(err);

    res.status(400).send({ success: false });
  } finally {
    connection.release();
  }
};

export { createProductHandler, createProductMiddlewares };
