import { _ } from "./pino";
import jwt from "jsonwebtoken";
import { Socket } from "socket.io";

interface SellerToken {
  id: string;
  role: string;
  email: string;
  avatar: string;
  country: string;
  username: string;
}

declare global {
  namespace Express {
    interface Request {
      sellerData: SellerToken;
    }
  }
}

const authMiddleware = (socket: Socket, next: (err?: Error) => void) => {
  const token = socket.handshake.query.token as string;

  console.log(token);

  if (!token) {
    return next(new Error("Authentication error"));
  }

  jwt.verify(token, process.env.SELLER_JWT_KEY!, (err: any, decoded: any) => {
    if (err) {
      return next(new Error("Authentication error"));
    }

    if (decoded.role !== "SELLER") {
      return next(new Error("Authentication error"));
    }

    (socket as any).user = decoded as SellerToken;

    next();
  });
};

export { authMiddleware, SellerToken };
