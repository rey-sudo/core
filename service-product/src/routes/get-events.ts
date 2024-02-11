import { Request, Response } from "express";
import { sellerMiddleware } from "../utils/seller";
import { requireAuth } from "../utils/required";
import { BadRequestError } from "../errors";
import { _ } from "../utils/pino";

const clients: any = {};

const getEventsMiddlewares: any = [sellerMiddleware, requireAuth];

const getEventsHandler = async (req: Request, res: Response) => {
  try {
    const SELLER = req.sellerData;

    if (clients.hasOwnProperty(SELLER.seller_id)) {
      delete clients[SELLER.seller_id];
    }

    res.setHeader("Content-Type", "text/event-stream");
    res.setHeader("Cache-Control", "no-cache");
    res.setHeader("Connection", "keep-alive");

    clients[SELLER.seller_id] = res;

    const scheme = {
      type: "connected",
      client: SELLER.seller_id,
      payload: "",
    };

    clients[SELLER.seller_id].write(`data: ${JSON.stringify(scheme)}\n\n`);

    req.on("close", () => {
      clients[SELLER.seller_id].end();
      delete clients[SELLER.seller_id];
    });
  } catch (err) {
    _.error(err);
    throw new BadRequestError("failed");
  } 
};

export { getEventsMiddlewares, getEventsHandler, clients };
