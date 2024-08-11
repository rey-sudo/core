import * as route from "./routes";
import DB from "./db";
import { app } from "./app";
import { catcher, check, checkpoint } from "./pod/index";
import { NotFoundError, errorMiddleware } from "./errors";
import serviceProductListener from "./kafka/products";
import compression from "compression";
import { eventBus } from "./db/redis";

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

    if (!process.env.SELLER_JWT_KEY) {
      throw new Error("SELLER_JWT_KEY error");
    }

    if (!process.env.USER_JWT_KEY) {
      throw new Error("USER_JWT_KEY error");
    }

    if (!process.env.TOKEN_EXPIRATION) {
      throw new Error("TOKEN_EXPIRATION error");
    }

    if (!process.env.EVENT_BUS_URI) {
      throw new Error("EVENT_BUS_URI error");
    }

    DB.connect({
      host: "mysql",
      port: 3306,
      user: "marketplace",
      password: "password",
      database: "service_gate",
    });

    await eventBus
      .connect({
        url: process.env.EVENT_BUS_URI,
        connectTimeout: 100000,
        keepAlive: 100000,
      })
      .then(() => console.log("eventBus connected"))
      .catch((err: any) => catcher(err));

    serviceProductListener();

    checkpoint("ready");

    const errorEvents: string[] = [
      "exit",
      "SIGINT",
      "SIGTERM",
      "SIGQUIT",
      "uncaughtException",
      "unhandledRejection",
    ];

    errorEvents.forEach((e: string) => process.on(e, (err) => catcher(err)));

    app.post(
      "/api/gateway/create-slot",

      route.createOrderMiddlewares,

      route.createOrderHandler
    );

    app.post(
      "/api/gateway/deploy",

      route.deployMiddlewares,

      route.deployHandler
    );

    app.post(
      "/api/gateway/locking-endpoint",

      route.lockingEndpointMiddlewares,

      route.lockingEndpointHandler
    );

    app.post(
      "/api/gateway/locking-tx",

      route.lockingTxMiddlewares,

      route.lockingTxHandler
    );

    app.get(
      "/api/gateway/get-slots",

      route.getSlotsMiddlewares,

      route.getSlotsHandler
    );

    app.get(
      "/api/gateway/get-slot/:id",

      route.getSlotMiddlewares,

      route.getSlotHandler
    );

    app.post(
      "/api/gateway/waiting-tx",

      route.startTxMiddlewares,

      route.startTxHandler
    );

    app.get(
      "/api/gateway/buy-options/:id",

      route.buyOptionsMiddlewares,

      route.buyOptionsHandler
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
