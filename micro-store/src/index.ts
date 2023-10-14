import mongoose from "mongoose";
import limiterStore from "./limiter/client";
import compression from "compression";
import cacheStore from "./cache/client";
import * as ROUTES from "./routes";
import { eventBus } from "./event-bus/client/client";
import { eventDriver } from "./event-driver/driver";
import { connHandler, errorHandler, setTimeOut } from "./pod/index";
import { app } from "./app";
import { NotFoundError, errorMiddleware, rateLimit } from "@alphaicterus/global";


const main = async () => {
  try {
    if (!process.env.EVENT_BUS_URI) {
      throw new Error("EVENT_BUS_URI must be defined");
    }

    if (!process.env.MONGO_DB_NAME) {
      throw new Error("MONGO_DB_NAME must be defined");
    }

    if (!process.env.MONGO_DB_URI) {
      throw new Error("MONGO_DB_URI must be defined");
    }

    if (!process.env.USERS_JWT_KEY) {
      throw new Error("USERS_JWT_KEY error");
    }

    if (!process.env.AUDITS_JWT_KEY) {
      throw new Error("AUDITS_JWT_KEY error");
    }

    if (!process.env.ADMIN_JWT_KEY) {
      throw new Error("ADMIN_JWT_KEY error");
    }

    if (!process.env.TOKEN_EXPIRATION_TIME) {
      throw new Error("TOKEN_EXPIRATION_TIME error");
    }

    if (!process.env.EXPRESS_PORT) {
      throw new Error("EXPRESS_PORT error");
    }

    if (!process.env.EXPRESS_TIMEOUT) {
      throw new Error("EXPRESS_TIMEOUT error");
    }

    if (!process.env.DRIVER_ROLE) {
      throw new Error("DRIVER_ROLE error");
    }

    if (!process.env.GENERAL_LIMIT_TIME) {
      throw new Error("GENERAL_LIMIT_TIME error");
    }

    if (!process.env.GENERAL_LIMIT_MAX) {
      throw new Error("GENERAL_LIMIT_MAX error");
    }

    if (!process.env.CRITICAL_LIMIT_TIME) {
      throw new Error("CRITICAL_LIMIT_TIME error");
    }

    if (!process.env.CRITICAL_LIMIT_MAX) {
      throw new Error("CRITICAL_LIMIT_MAX error");
    }

    if (!process.env.LIMITER_STORE) {
      throw new Error("LIMITER_STORE error");
    }

    if (!process.env.CORS_DOMAINS) {
      throw new Error("CORS_DOMAINS error");
    }

    if (!process.env.CACHE_STORE) {
      throw new Error("CACHE_STORE error");
    }

    if (!process.env.CACHE_INTERVAL) {
      throw new Error("CACHE_INTERVAL error");
    }

    ///////////////////////////////////////////////////

    await eventBus
      .connect({
        url: process.env.EVENT_BUS_URI,
        connectTimeout: 100000,
        keepAlive: 100000,
      })
      .then(() => connHandler("eventBus"))
      .catch((e: any) => errorHandler("BUS_CONN", e));

    await mongoose
      .connect(process.env.MONGO_DB_URI, {
        connectTimeoutMS: 60000,
        socketTimeoutMS: 60000,
        maxPoolSize: 10000,
        minPoolSize: 100,
        dbName: process.env.MONGO_DB_NAME,
        autoIndex: false,
        ssl: true,
        sslValidate: false,
        sslCA: `${__dirname}/cert.pem`
      })
      .then(() => connHandler("mongoose"))
      .catch((e) => errorHandler("MONGO_CONN", e));

    await eventDriver
      .subscribe(eventBus.client, process.env.DRIVER_ROLE)
      .then(() => connHandler("eventDriver"))
      .catch((e) => errorHandler("DRIVER_CONN", e));

    await limiterStore
      .connect({
        url: process.env.LIMITER_STORE,
        connectTimeout: 100000,
        keepAlive: 100000,
      })
      .then(() => connHandler("limiterStore"))
      .catch((e: any) => errorHandler("LIMITER_CONN", e));

    await cacheStore
      .connect({
        url: process.env.CACHE_STORE,
        connectTimeout: 100000,
        keepAlive: 100000,
      })
      .then(() => connHandler("cacheStore"))
      .catch((e: any) => errorHandler("CACHE_CONN", e));

    ///////////////////////////////////////////////////

    eventBus.client.on("end", (e: any) => errorHandler("BUS_END", e));

    eventBus.client.on("error", (e: any) => errorHandler("BUS_ERROR", e, true));

    mongoose.connection.on("error", (e) => errorHandler("MONGO_ERROR", e));

    mongoose.connection.on("close", (e) => errorHandler("MONGO_CLOSE", e));

    eventDriver.on("error", (e: any) => errorHandler("DRIVER_ERROR", e));

    limiterStore.client.on("end", (e: any) => errorHandler("LIMITER_END", e));

    limiterStore.client.on("error", (e: any) =>
      errorHandler("LIMITER_ERROR", e)
    );

    process.on("exit", (e) => errorHandler(e));

    process.on("SIGINT", (e) => errorHandler(e));

    process.on("SIGTERM", (e) => errorHandler(e));

    process.on("SIGQUIT", (e) => errorHandler(e));

    process.on("uncaughtException", (e) => errorHandler(e));

    process.on("unhandledRejection", (e) => errorHandler(e));

    ////////////////////////////////////////////////////

    app.post(
      "/api/store/create-product",

      rateLimit(limiterStore.client, {
        path: "create-product",
        windowMs: process.env.GENERAL_LIMIT_TIME,
        max: process.env.GENERAL_LIMIT_MAX,
      }),

      ...ROUTES.createProductMiddlewares,

      ROUTES.createProductHandler
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
