import { Request, Response } from "express";
import { sellerMiddleware } from "../utils/seller";
import { requireAuth } from "../utils/required";
import { BadRequestError } from "../errors";
import { _ } from "../utils/pino";
import { eventBus } from "../db/redis";

const clients: any = {};

const sendEvent = (clientId: string, type: string, payload?: any) => {
  if (clients.hasOwnProperty(clientId)) {
    const scheme = {
      type: type,
      client: clientId,
      payload: payload,
    };

    clients[clientId].write(`data: ${JSON.stringify(scheme)}\n\n`);
  }
};

//const getEventsMiddlewares: any = [sellerMiddleware, requireAuth];

const getEventsMiddlewares: any = [];

const getEventsHandler = async (req: Request, res: Response) => {
  try {
    console.log("YES");

    const SELLER = {
      id: "KD6HQNRE00PNVJGU5M58",
    };

    if (clients.hasOwnProperty(SELLER.id)) {
      delete clients[SELLER.id];
    }

    res.setHeader("Content-Type", "text/event-stream");
    res.setHeader("Cache-Control", "no-cache");
    res.setHeader("Connection", "keep-alive");

    const response = {
      type: "connected",
      client: SELLER.id,
      payload: "",
    };

    res.write(`data: ${JSON.stringify(response)}\n\n`);

    const sendPing = setInterval(() => {
      res.write("event: ping\n");
      res.write("data: {}\n\n");
    }, 29000);

    clients[SELLER.id] = res;

    req.on("close", () => {
      res.end();
      delete clients[SELLER.id];
      clearInterval(sendPing);
    });

    const listener = (message: any, channel: any) => {
      console.log(message, channel);
    };

    await eventBus.client.psubscribe(SELLER.id, listener);
  } catch (err) {
    _.error(err);
    throw new BadRequestError("failed");
  }
};

export { getEventsMiddlewares, getEventsHandler, sendEvent, clients };
