import { Request, Response, NextFunction } from "express";

declare global {
  namespace Express {
    interface Request {
      clientIp?: any;
    }
  }
}

export const requestIp = (req: Request, res: Response, next: NextFunction) => {
  req.clientIp = req.headers["cf-connecting-ip"] || req.ip;
  next();
};
