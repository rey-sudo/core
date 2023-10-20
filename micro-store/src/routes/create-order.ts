import { BadRequestError, expressBodyValidator } from "@alphaicterus/global";
import { Request, Response } from "express";
import { CREATE_ORDER } from "../utils/body-validator";
import { createOrder } from "./transactions/create-order";
import { Product } from "../models";

const createOrderMiddlewares = [CREATE_ORDER, expressBodyValidator];

/**ADMIN | HANDLER | POST
 *
 * Creates a new order
 */
const createOrderHandler = async (req: Request, res: Response) => {

  const findProduct = await Product.findOne({ pid: req.body.product_pid })

  if (!findProduct) {
    throw new BadRequestError("NO_PRODUCT");
  }

  const orderCreated = await createOrder(req.body);

  if (!orderCreated) {
    throw new BadRequestError("CREATE_ERROR");
  }

  res.status(200).send(orderCreated);
};

export { createOrderMiddlewares, createOrderHandler };
