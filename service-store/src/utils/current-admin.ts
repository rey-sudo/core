import { Request, Response, NextFunction } from "express";
import jwt from "jsonwebtoken";
import { _ } from "./logger";

interface AdminPayLoad {
  pid: string;
  scope: string;
  entity: string;
  email: string;
  username: string;
}

declare global {
  namespace Express {
    interface Request {
      currentAdmin: AdminPayLoad;
    }
  }
}

export const currentAdmin = (
  req: Request,
  res: Response,
  next: NextFunction
) => {
  try {
    if (!req.session?.jwt) {
      throw new Error("NO_SESSION");
    }

    const SESSION = jwt.verify(
      req.session.jwt,
      process.env.ADMIN_JWT_KEY!
    ) as AdminPayLoad;

    if (SESSION.entity !== "admin") {
      throw new Error("WRONG_ENTITY");
    }

    req.currentAdmin = SESSION;
  } catch (err) {}

  next();
};
