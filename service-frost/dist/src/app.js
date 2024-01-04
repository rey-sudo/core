"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.app = void 0;
require("express-async-errors");
const express_1 = __importDefault(require("express"));
const cors_1 = __importDefault(require("cors"));
const helmet_1 = __importDefault(require("helmet"));
const cookie_session_1 = __importDefault(require("cookie-session"));
const body_parser_1 = require("body-parser");
const global_1 = require("../global");
const app = (0, express_1.default)();
exports.app = app;
const corsOrigin = process.env.CORS_DOMAINS;
const corsOptions = {
    origin: corsOrigin?.split(",") || "*",
    methods: ["GET", "POST"],
    credentials: true,
    maxAge: 86400,
    preflightContinue: false,
    exposedHeaders: ["Set-Cookie"],
    optionsSuccessStatus: 204,
};
const sessionOptions = {
    maxAge: 168 * 60 * 60 * 1000,
    signed: false,
    secureProxy: true,
    httpOnly: true,
    sameSite: "strict",
};
app.set("trust proxy", 1);
app.use((0, helmet_1.default)());
app.use((0, cors_1.default)(corsOptions));
app.use(global_1.requestIp);
app.use((0, body_parser_1.urlencoded)({ extended: true, parameterLimit: 15 }));
app.use((0, body_parser_1.json)({ limit: 5000000 }));
app.use((0, cookie_session_1.default)(sessionOptions));
