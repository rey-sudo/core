import { BadRequestError, expressBodyValidator } from "@alphaicterus/global";
import { Request, Response } from "express";
import { Report } from "../models/report";
import { GET_ALL_REPORTS } from "../utils/body-validator";
import { Product } from "../models";
import { createProduct } from "./transactions/create-product";

const createProductMiddlewares = [ expressBodyValidator];

/**ADMIN | HANDLER | POST
 *
 * 
 *
 */
const createProductHandler = async (req: Request, res: Response) => {

  const productCreated = await createProduct(req.body);


  if (!productCreated.success) {
    throw new BadRequestError("CREATE_ERROR");
  }

  res.status(200).send(productCreated);
};

export { createProductMiddlewares, createProductHandler  };
