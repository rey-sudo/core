import DB from "../db";
import uploadMiddleware from "../utils/multer";
import { BadRequestError } from "../errors";
import { Request, Response } from "express";
import { getImageId } from "../utils/nano";
import { sellerMiddleware } from "../utils/seller";
import { requireAuth } from "../utils/required";
import { _ } from "../utils/pino";

const createImageMiddlewares: any = [
  sellerMiddleware,
  requireAuth,
  uploadMiddleware.array("image", 5),
];

const createImageHandler = async (req: Request, res: Response) => {
  const SELLER = req.sellerData;

  let connection: any = null;

  let response: string[] = [];

  try {
    if (!req.files) {
      throw new Error("NON_FILES");
    }

    connection = await DB.client.getConnection();

    await connection.beginTransaction();

    for (const image of req.files as Express.Multer.File[]) {
      const schemeData = `
      INSERT INTO media (
        media_id,
        seller_id,
        media_type,
        media_data,
        schema_v
       ) VALUES (?, ?, ?, ?, ?)`;

      const image_id = getImageId();

      const schemeValue = [
        image_id,
        SELLER.seller_id,
        "image",
        image.buffer,
        0,
      ];

      const [result] = await connection.execute(schemeData, schemeValue);

      if (result.affectedRows === 1) {
        response.push(image_id);
      }
    }

    await connection.commit();

    res.status(200).send({ success: true, payload: response });
  } catch (err) {
    await connection.rollback();

    _.error(err);

    throw new BadRequestError("failed");
  } finally {
    connection.release();
  }
};

export { createImageMiddlewares, createImageHandler };
