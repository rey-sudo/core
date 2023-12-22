import { NotAuthorizedError } from "@alphaicterus/global";
import { Request, Response, NextFunction } from "express";


export const auditorRequiredAuth = (
  req: Request,
  res: Response,
  next: NextFunction
) => {
  if (!req.currentAuditor) {
    throw new NotAuthorizedError();
  }

  next();
};
