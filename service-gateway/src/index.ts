import "express-async-errors";
import express from "express";
import cors from "cors";
import helmet from "helmet";
import cookieSession from "cookie-session";
import pkg from 'body-parser';
const { json, urlencoded } = pkg;
import { Request, Response, NextFunction } from "express";

const app = express();

const corsOrigin = process.env.CORS_DOMAINS!;

const corsOptions = {
    origin: corsOrigin?.split(",") || "*",
    methods: ["GET", "POST"],
    credentials: true,
    maxAge: 86400,
    preflightContinue: false,
    exposedHeaders: ["Set-Cookie"],
    optionsSuccessStatus: 204,
};

const sessionOptions: object = {
    maxAge: 168 * 60 * 60 * 1000,
    signed: false,
    secure: true,
    httpOnly: true,
    sameSite: "strict",
};

declare global {
    namespace Express {
        interface Request {
            publicAddress?: any;
        }
    }
}

const getPublicAddress = (req: Request, res: Response, next: NextFunction) => {
    req.publicAddress = req.headers["cf-connecting-ip"] || req.ip;
    next();
};

app.set("trust proxy", 1);

app.use(helmet());

app.use(cors(corsOptions));

app.use(getPublicAddress);

app.use(urlencoded({ extended: true, parameterLimit: 15 }));

app.use(json({ limit: 5000000 }));

app.use(cookieSession(sessionOptions));

const message: string = "Hello, TypeScript with Node.js!";
console.log(message);
