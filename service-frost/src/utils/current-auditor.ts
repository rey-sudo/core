import { Request, Response, NextFunction } from "express";
import jwt from "jsonwebtoken";

interface AuditorPayLoad {
  scope: string;
  entity: string;
  pid: string;
  email: string;
  username: string;
}

declare global {
  namespace Express {
    interface Request {
      currentAuditor: AuditorPayLoad;
    }
  }
}

export const currentAuditor = (
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
        process.env.AUDITS_JWT_KEY!
      ) as AuditorPayLoad;

      if (tokenPay.entity !== "auditor") {
        return next();
      }

      req.currentAuditor = tokenPay;
    } else {
      const sessionPay = jwt.verify(
        req.session.jwt,
        process.env.AUDITS_JWT_KEY!
      ) as AuditorPayLoad;

      if (sessionPay.entity !== "auditor") {
        return next();
      }

      req.currentAuditor = sessionPay;
    }
  } catch (err) {}

  next();
};
