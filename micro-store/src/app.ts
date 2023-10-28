import express from "express";
import "express-async-errors";
import cors from "cors";
import helmet from "helmet";
import cookieSession from "cookie-session";
import { json, urlencoded } from "body-parser";
import { requestIp } from "@alphaicterus/global";

const app = express();

const corsOptions = {
  origin: process.env.CORS_DOMAINS!.split(","),
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
  secureProxy: true,
  httpOnly: true,
  sameSite: "strict",
};

app.set("trust proxy", 1);

app.use(helmet());

app.use(cors(corsOptions));

app.use(requestIp);

app.use(urlencoded({ extended: true, parameterLimit: 15 }));

app.use(json({ limit: 5000000 }));

app.use(cookieSession(sessionOptions));

export { app };

//