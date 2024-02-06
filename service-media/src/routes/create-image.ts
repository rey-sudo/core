import DB from "../db";
import API from "../api";
import multer from "multer";
import { BadRequestError } from "../errors";
import { Request, Response } from "express";
import { getImageId } from "../utils/nano";
import { sellerMiddleware } from "../utils/seller";
import { requireAuth } from "../utils/required";
import { _ } from "../utils/pino";

const storage = multer.memoryStorage();

const upload = multer({
  storage: storage,
  fileFilter: function (req, file, callback) {
    console.log("FILTER", file);
    const whitelist = ["image/png", "image/jpeg", "image/gif", "image/webp"];

    if (!whitelist.includes(file.mimetype)) {
      return callback(null, false);
    }

    if (!file.originalname.match(/\.(jpg|jpeg|png|gif)$/)) {
      return callback(null, false);
    }
    //mimetype
    callback(null, true);
  },
});

const createImageMiddlewares: any = [
  // sellerMiddleware,
  // requireAuth,
  upload.array("image", 5),
];

const createImageHandler = async (req: Request, res: Response) => {
  const SELLER = req.sellerData;

  let connection: any = null;

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

      const schemeValue = [
        getImageId(),
        "deletethis", //
        "image",
        image.buffer,
        0,
      ];

      await connection.execute(schemeData, schemeValue);
    }

    await connection.commit();

    res.status(200).send({ success: true });
  } catch (err) {
    await connection.rollback();

    _.error(err);

    throw new BadRequestError("failed");
  } finally {
    connection.release();
  }
};

export { createImageMiddlewares, createImageHandler };
