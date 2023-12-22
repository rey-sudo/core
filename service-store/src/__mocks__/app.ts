import express from "express";
import "express-async-errors";
import cors from "cors";
import helmet from "helmet";
import cookieSession from "cookie-session";
import { json, urlencoded } from "body-parser";
import { NotFoundError, errorMiddleware, requestIp } from "@alphaicterus/global";
import {
  createAuditorHandler,
  createAuditorMiddlewares,
  createGroupsHandler,
  createGroupsMiddlewares,
  createPrevoteHandler,
  createPrevoteMiddlewares,
  createReportHandler,
  createReportMiddlewares,
  createReviewHandler,
  createReviewMiddlewares,
  createRoundHandler,
  createRoundMiddlewares,
  createSubscriptionHandler,
  createSubscriptionMiddlewares,
  currentAuditorHandler,
  currentAuditorMiddlewares,
  loginAuditorHandler,
  loginAuditorMiddlewares,
  resetAuditingHandler,
  resetAuditingMiddlewares,
  resetGovernanceHandler,
  resetGovernanceMiddlewares,
  resetSelectionHandler,
  resetSelectionMiddlewares,
  simulateDaovotesHandler,
  simulateDaovotesMiddlewares,
  startAuditingHandler,
  startAuditingMiddlewares,
  startGovernanceHandler,
  startGovernanceMiddlewares,
  startSelectionHandler,
  startSelectionMiddlewares,
} from "../routes";

const app = express();

const compression = require("compression");

const corsOptions = {
  origin: [
    "https://app.scatdao.com",
    "http://api.scatdao.dev",
    "http://localhost:8080",
  ],
  credentials: true,
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

app.use(helmet());

app.set("trust proxy", 1);

app.use(cors(corsOptions));

app.use(requestIp);

app.use(urlencoded({ extended: true }));

app.use(json({ limit: 100000 }));

app.use(cookieSession(sessionOptions));

app.post(
  "/api/audits/create-round",

  ...createRoundMiddlewares,

  createRoundHandler
);

app.post(
  "/api/audits/create-auditor",

  ...createAuditorMiddlewares,

  createAuditorHandler
);

app.post(
  "/api/audits/create-groups",

  ...createGroupsMiddlewares,

  createGroupsHandler
);

app.post(
  "/api/audits/start-selection",

  ...startSelectionMiddlewares,

  startSelectionHandler
);

app.post(
  "/api/audits/create-prevote",

  ...createPrevoteMiddlewares,

  createPrevoteHandler
);

app.post(
  "/api/audits/create-subscription",

  ...createSubscriptionMiddlewares,

  createSubscriptionHandler
);

app.post(
  "/api/audits/start-governance",

  ...startGovernanceMiddlewares,

  startGovernanceHandler
);

app.get(
  "/api/audits/reset-selection",

  ...resetSelectionMiddlewares,

  resetSelectionHandler
);

app.get(
  "/api/audits/reset-governance",

  ...resetGovernanceMiddlewares,

  resetGovernanceHandler
);

app.get(
  "/api/audits/reset-auditing",

  ...resetAuditingMiddlewares,

  resetAuditingHandler
);

app.post(
  "/api/audits/simulate-daovotes",

  ...simulateDaovotesMiddlewares,

  simulateDaovotesHandler
);

app.post(
  "/api/audits/start-auditing",

  ...startAuditingMiddlewares,

  startAuditingHandler
);

app.post(
  "/api/audits/create-report",

  ...createReportMiddlewares,

  createReportHandler
);

app.post(
  "/api/audits/create-review",

  ...createReviewMiddlewares,

  createReviewHandler
);

app.post(
  "/api/audits/login-auditor",

  ...loginAuditorMiddlewares,

  loginAuditorHandler
);

app.get(
  "/api/audits/get-current-auditor",

  ...currentAuditorMiddlewares,

  currentAuditorHandler
);


app.all("*", (_req, _res) => {
  throw new NotFoundError();
});

app.use(errorMiddleware);

app.use(compression());

export { app };
