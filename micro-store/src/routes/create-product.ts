import { BadRequestError, expressBodyValidator } from "@alphaicterus/global";
import { Request, Response } from "express";
import { CREATE_PRODUCT } from "../utils/body-validator";
import { createProduct } from "./transactions/create-product";

const createProductMiddlewares = [ CREATE_PRODUCT, expressBodyValidator];

/**ADMIN | HANDLER | POST
 * 
 * Creates a new product
 */
const createProductHandler = async (req: Request, res: Response) => {

  const productCreated = await createProduct(req.body);


  if (!productCreated.success) {
    throw new BadRequestError("CREATE_ERROR");
  }

  res.status(200).send(productCreated);
};

export { createProductMiddlewares, createProductHandler  };
