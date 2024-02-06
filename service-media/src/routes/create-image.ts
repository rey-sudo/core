import DB from "../db";
import API from "../api";
import multer from "multer";
import { BadRequestError } from "../errors";
import { Request, Response } from "express";
import { getSlotId } from "../utils/nano";
import { sellerMiddleware } from "../utils/seller";
import { requireAuth } from "../utils/required";
import { _ } from "../utils/pino";

const storage = multer.memoryStorage();

const upload = multer({
  storage: storage,
  fileFilter: function (req, file, callback) {
    console.log("FILTER");
    if (!file.originalname.match(/\.(jpg|jpeg|png|gif)$/)) {
      return callback(null, false);
    }

    callback(null, true);
  },
});

const createImageMiddlewares: any = [
  // sellerMiddleware,
  // requireAuth,
  upload.array("image[]", 5),
];

const createImageHandler = async (req: Request, res: Response) => {
  console.log(req.files);
  res.status(200).send({ success: true });
};

export { createImageMiddlewares, createImageHandler };
