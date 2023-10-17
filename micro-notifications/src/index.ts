import mongoose from "mongoose";
import { eventBus } from "./event-bus/client/client";
import { MicroStoreListener } from "./event-bus/listeners/service-audits-listener";
import { connHandler, errorHandler, setTimeOut } from "./pod/index";
import { _ } from "./utils/logger";

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

    if (!process.env.DRIVER_ROLE) {
      throw new Error("DRIVER_ROLE error");
    }

    if (!process.env.REDIS_EXPIRATION) {
      throw new Error("REDIS_EXPIRATION error");
    }
    

    await eventBus
      .connect({
        url: process.env.EVENT_BUS_URI,
        connectTimeout: 100000,
        keepAlive: 100000,
      })
      .then(() => connHandler("eventBus"))
      .catch((e: any) => errorHandler("ERROR_BUS_CLIENT", e));

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
      .catch((e) => errorHandler("ERR_MONGO_CONN", e));

    mongoose.set("strictQuery", true);

    new MicroStoreListener(eventBus.client)
      .listen()
      .then((e: any) =>
        e.on("error", (e: any) => errorHandler("ERR_LISTENER_CONN", e))
      );

    eventBus.client.on("end", (e: any) => errorHandler("ERROR_BUS_END", e));

    eventBus.client.on("error", (e: any) =>
      errorHandler("ERROR_BUS_ERR", e, true)
    );

    mongoose.connection.on("error", (e) => errorHandler("ERROR_MONGO_ERR", e));

    mongoose.connection.on("close", (e) => errorHandler("ERROR_MONGO_CLOSE", e));

    process.on("exit", (e) => errorHandler(e));

    process.on("SIGINT", (e) => errorHandler(e));

    process.on("SIGTERM", (e) => errorHandler(e));

    process.on("SIGQUIT", (e) => errorHandler(e));

    process.on("uncaughtException", (e) => errorHandler(e));
  } catch (e) {
    errorHandler(e);
  }
  setTimeOut();
};

main();
