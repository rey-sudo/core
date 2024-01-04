import compression from "compression";
import { errorHandler, setTimeOut } from "./pod/index";
import { app } from "./app";
import { NotFoundError, errorMiddleware, rateLimit } from "../global";
import {
  auditorInformationHandler,
  auditorInformationMiddlewares,
  createRoundHandler,
  createRoundMiddlewares,
} from "./routes";

const main = async () => {
  try {
    
    if (!process.env.EXPRESS_PORT) {
      throw new Error("EXPRESS_PORT error");
    }

    if (!process.env.EXPRESS_TIMEOUT) {
      throw new Error("EXPRESS_TIMEOUT error");
    }

    if (!process.env.CORS_DOMAINS) {
      throw new Error("CORS_DOMAINS error");
    }

    ///////////////////////////////////////////////////

    process.on("exit", (e) => errorHandler(e));

    process.on("SIGINT", (e) => errorHandler(e));

    process.on("SIGTERM", (e) => errorHandler(e));

    process.on("SIGQUIT", (e) => errorHandler(e));

    process.on("uncaughtException", (e) => errorHandler(e));

    process.on("unhandledRejection", (e) => errorHandler(e));

    ////////////////////////////////////////////////////

    app.post(
      "/api/audits/create-round",
      
      createRoundMiddlewares,

      createRoundHandler
    );

    app.get(
      "/api/audits/get-auditor-information",
      
      auditorInformationMiddlewares,

      auditorInformationHandler
    );

    app.all("*", (_req, _res) => {
      throw new NotFoundError();
    });

    app.use(errorMiddleware);

    app.use(compression());
  } catch (e) {
    errorHandler(e);
  }
  setTimeOut();
};

main();
