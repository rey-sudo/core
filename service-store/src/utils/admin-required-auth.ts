import { NotAuthorizedError } from "@alphaicterus/global";
import { Request, Response, NextFunction } from "express";


export const adminRequiredAuth = (
  req: Request,
  res: Response,
  next: NextFunction
) => {
  if (!req.currentAdmin) {
    throw new NotAuthorizedError();
  }

  next();
};
