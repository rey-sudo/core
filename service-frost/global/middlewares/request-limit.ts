import { TooManyRequests } from "../errors/TooManyRequests";
import { Request, Response, NextFunction } from "express";
import { RedisClientType } from "redis";
const _ = require("pino")();

interface RateLimitOptions {
  windowMs: string;
  max: string;
  path: string;
}
//////////

export const rateLimit = (
  store: RedisClientType,
  options: RateLimitOptions
) => {
  return async (req: Request, res: Response, next: NextFunction) => {
    try {
      const payload = `${req.clientIp}:${options.path}`;

      await store
        .multi()
        .setNX(payload, options.max)
        .expire(payload, parseInt(options.windowMs))
        .decrBy(payload, 1)
        .exec();

      const remaining = await store.get(payload);

      if (!remaining) {
        throw new Error();
      }

      if (parseInt(remaining) < 1) {
        throw new Error();
      }

      res.set({ "rate-limit-remaining": remaining });
    } catch (e) {
      throw new TooManyRequests("limit");
    }

    next();
  };
};
