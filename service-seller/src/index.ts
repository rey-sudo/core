import * as route from "./routes";
import { catcher, check, checkpoint } from "./pod/index";
import { NotFoundError, errorMiddleware } from "../global";
import { app } from "./app";
import compression from "compression";
import DB from "./db";

const main = async () => {
  try {
    if (!process.env.POD_TIMEOUT) {
      throw new Error("POD_TIMEOUT error");
    }

    if (!process.env.EXPRESS_PORT) {
      throw new Error("EXPRESS_PORT error");
    }

    if (!process.env.EXPRESS_TIMEOUT) {
      throw new Error("EXPRESS_TIMEOUT error");
    }

    if (!process.env.CORS_DOMAINS) {
      throw new Error("CORS_DOMAINS error");
    }

    DB.connect({
      host: "10.96.222.125",
      port: 3306,
      user: "user",
      password: "",
      database: "service-seller",
    });

    checkpoint("ready");

    process.on("exit", (e) => catcher(e));

    process.on("SIGINT", (e) => catcher(e));

    process.on("SIGTERM", (e) => catcher(e));

    process.on("SIGQUIT", (e) => catcher(e));

    process.on("uncaughtException", (e) => catcher(e));

    process.on("unhandledRejection", (e) => catcher(e));

    app.post(
      "/api/seller/create-seller",

      route.createSellerMiddlewares,

      route.createSellerHandler
    );

    app.get(
      "/api/seller/current-seller",

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
