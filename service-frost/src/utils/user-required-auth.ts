import { NotAuthorizedError } from "@scatdao1/common";
import { Request, Response, NextFunction } from "express";


export const userRequiredAuth = (
  req: Request,
  res: Response,
  next: NextFunction
) => {
  if (!req.currentUser) {
    throw new NotAuthorizedError();
  }

  next();
};
