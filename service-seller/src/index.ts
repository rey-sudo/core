import * as route from "./routes";
import { catcher, check, checkpoint } from "./pod/index";
import { NotFoundError, errorMiddleware } from "../global";
import { app } from "./app";
import blockfrost from "./client";
import compression from "compression";

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

    if (!process.env.POD_TIMEOUT) {
      throw new Error("POD_TIMEOUT error");
    }

    blockfrost.connect({
      projectId: "previewXgODba40jVJAs1QgKTBOAuwhvNFHHMVo",
    });

    checkpoint("ready");

    process.on("exit", (e) => catcher(e));

    process.on("SIGINT", (e) => catcher(e));

    process.on("SIGTERM", (e) => catcher(e));

    process.on("SIGQUIT", (e) => catcher(e));

    process.on("uncaughtException", (e) => catcher(e));

    process.on("unhandledRejection", (e) => catcher(e));

    app.post(
      "/api/audits/create-round",

      route.createRoundMiddlewares,

      route.createRoundHandler
    );

    app.get(
      "/api/seller",

      route.getAddressUtxos,

      route.getAddressUtxosHandler
    );

    app.all("*", (_req, _res) => {
      throw new NotFoundError();
    });

    app.use(errorMiddleware);

    app.use(compression());
  } catch (e) {
    catcher(e);
  }
  check();
};

main();
