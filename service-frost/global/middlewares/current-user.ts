import { Request, Response, NextFunction } from "express";
import jwt from "jsonwebtoken";

interface UserPayLoad {
  scope: string;
  entity: string;
  pid: string;
  email: string;
  username: string;
}

declare global {
  namespace Express {
    interface Request {
      currentUser: UserPayLoad;
    }
  }
}

export const currentUser = (
  req: Request,
  res: Response,
  next: NextFunction
) => {
  if (!req.session?.jwt) {
    return next();
  }

  try {
    if (req.body.token) {
      const tokenPay = jwt.verify(
        req.body.token,
        process.env.JWT_KEY!
      ) as UserPayLoad;

      if (tokenPay.entity !== "user") {
        return next();
      } 
      
      req.currentUser = tokenPay;
    } else {
      const sessionPay = jwt.verify(
        req.session.jwt,
        process.env.JWT_KEY!
      ) as UserPayLoad;


      if (sessionPay.entity !== "user") {
        return next();
      }

      req.currentUser = sessionPay;
    }
  } catch (err) {}

  next();
};
