import { BadRequestError, expressBodyValidator } from "@alphaicterus/global";
import { Request, Response } from "express";
import { CREATE_PRODUCT } from "../utils/body-validator";
import { Product } from "../models";

const getProductMiddlewares = [ expressBodyValidator];

/**USER | HANDLER | GET
 * 
 * get a product
 */
const getProductHandler = async (req: Request, res: Response) => {

  const { pid } = req.query;

  const findProduct = await Product.findOne({ pid: pid });

  if (!findProduct) {
    throw new BadRequestError("NO_PRODUCT");
  }

  res.status(200).send(findProduct);
};

export { getProductMiddlewares, getProductHandler  };
