import "express-async-errors";
import express from "express";
import cors from "cors";
import helmet from "helmet";
import { json, urlencoded } from "body-parser";
import { getPublicAddress } from "./utils/address";
const app = express();
const corsOrigin = process.env.CORS_DOMAINS || "*";
const corsOptions = {
    origin: corsOrigin?.split(",") || "*",
    methods: ["GET", "POST"],
    credentials: true,
    maxAge: 86400,
    preflightContinue: false,
    exposedHeaders: ["Set-Cookie"],
    optionsSuccessStatus: 204,
};
app.set("trust proxy", 1);
app.use(helmet());
app.use(cors(corsOptions));
app.use(getPublicAddress);
app.use(urlencoded({ extended: true, parameterLimit: 15 }));
app.use(json({ limit: 5000000 }));
export { app };
