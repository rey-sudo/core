import { Request, Response, NextFunction } from "express";
import jwt from "jsonwebtoken";
import { NotAuthorizedError } from "../errors/NotAuthorizedError";

export interface TokenPayLoad {
  scope: string;
  entity: string;
  username: string;
  email: string;
  pid: string;
}

export const verifyToken = async (
  req: Request,
  res: Response,
  next: NextFunction
) => {
  if (!req.params.token) throw new NotAuthorizedError();

  try {
    const payload = jwt.verify(
      req.params.token,
      process.env.JWT_KEY!
    ) as TokenPayLoad;

    if (!payload) throw new NotAuthorizedError();

    req.currentUser = payload;
  } catch (err) {
    throw new NotAuthorizedError();
  }

  next();
};
