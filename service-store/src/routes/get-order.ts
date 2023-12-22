import { BadRequestError, expressBodyValidator } from "@alphaicterus/global";
import { Request, Response } from "express";
import { GET_ORDER } from "../utils/body-validator";
import { Order } from "../models";

const getOrderMiddlewares = [GET_ORDER, expressBodyValidator];

/**USER | HANDLER | GET
 *
 * get a order
 */
const getOrderHandler = async (req: Request, res: Response) => {
  const { pid } = req.query;

  const findOrder = await Order.findOne({ pid: pid }).populate({
    path: 'product',
    select: { pid: 1, name: 1 }
  });

  if (!findOrder) {
    throw new BadRequestError("NO_ORDER");
  }

  res.status(200).send(findOrder);
};

export { getOrderMiddlewares, getOrderHandler };
