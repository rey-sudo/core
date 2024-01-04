import { NotAuthorizedError } from "@scatdao1/common";
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
