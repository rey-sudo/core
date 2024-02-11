import { Request, Response } from "express";
import { sellerMiddleware } from "../utils/seller";
import { requireAuth } from "../utils/required";

const clients: any = {};

const getEventsMiddlewares: any = [sellerMiddleware, requireAuth];

const getEventsHandler = async (req: Request, res: Response) => {
  console.log(clients);
  const SELLER = req.sellerData;

  res.setHeader("Content-Type", "text/event-stream");
  res.setHeader("Cache-Control", "no-cache");
  res.setHeader("Connection", "keep-alive");

  clients[SELLER.seller_id] = res;

  res.write(`event:connected;client:${SELLER.seller_id};`);

  req.on("close", () => {
    delete clients[SELLER.seller_id];
  });
};

export { getEventsMiddlewares, getEventsHandler, clients };
