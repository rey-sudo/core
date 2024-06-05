import { Request, Response } from "express";
import { sendEvent } from "./get-events";

const sendEventsMiddlewares: any = [];

const sendEventsHandler = async (req: Request, res: Response) => {
  sendEvent("KD6HQNRE00PNVJGU5M58", "gate:new", { what: "yes" });
  res.json({ success: true });
};

export { sendEventsMiddlewares, sendEventsHandler };
