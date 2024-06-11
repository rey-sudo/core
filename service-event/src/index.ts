import * as route from "./routes";
import DB from "./db";
import { app } from "./app";
import { catcher, check, checkpoint } from "./pod/index";
import { NotFoundError, errorMiddleware } from "./errors";
import serviceProductListener from "./kafka/service-product";
import compression from "compression";
import "dotenv/config";
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

    if (!process.env.TOKEN_EXPIRATION) {
      throw new Error("TOKEN_EXPIRATION error");
    }

    if (!process.env.EVENT_BUS_URI) {
      throw new Error("EVENT_BUS_URI error");
    }

    /* DB.connect({
      host: "mysql",
      port: 3306,
      user: "marketplace",
      password: "password",
      database: "service_gate",
    });
*/
    //serviceProductListener();

    await eventBus
      .connect({
        url: process.env.EVENT_BUS_URI,
        connectTimeout: 100000,
        keepAlive: 100000,
      })
      .then(() => console.log("eventBus connected"))
      .catch((err: any) => catcher(err));

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

    app.get(
      "/api/event/get-events",

      route.getEventsMiddlewares,

      route.getEventsHandler
    );

    app.post(
      "/api/event/send-event",

      route.sendEventsMiddlewares,

      route.sendEventsHandler
    );

    app.get("/api/event/healthcheck", (req, res) => {
      res.status(200).json({ status: "Test OK" });
    });

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
