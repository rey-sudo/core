
import { mongoWrapper } from "./event-driver/client/mongo";
import { eventBus } from "./event-bus/client/client";
import { eventDriver } from "./event-driver/driver";
import { errorHandler, connHandler, setTimeOut } from "./pod/index";
import { _ } from "./utils/logger";
import { ERROR } from "@alphaicterus/global";

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

    if (!process.env.PUBLISHER_ROLE) {
      throw new Error("PUBLISHER_ROLE must be defined");
    }

    if (!process.env.PUBLISHER_NAME) {
      throw new Error("PUBLISHER_NAME must be defined");
    }

    if (!process.env.PUBLISHER_TOTAL) {
      throw new Error("PUBLISHER_TOTAL must be defined");
    }


    await eventBus
      .connect({
        url: process.env.EVENT_BUS_URI,
        connectTimeout: 100000,
        keepAlive: 100000,
      })
      .then(() => connHandler("eventBus"))
      .catch((e: any) => errorHandler(ERROR.BUS100, e));


    await mongoWrapper
      .connect(process.env.MONGO_DB_URI, {
        connectTimeoutMS: 100000,
        socketTimeoutMS: 100000,
        maxPoolSize: 10000,
        minPoolSize: 100,
        dbName: process.env.MONGO_DB_NAME,
        ssl: true,
        sslValidate: false,
        sslCA: `${__dirname}/cert.pem`
      })
      .then(() => connHandler("mongoWrapper"))
      .catch((e) => errorHandler(ERROR.MONGO100, e));


 
    await eventDriver
      .connect(mongoWrapper.client, eventBus.client, {
        driverRole: process.env.PUBLISHER_ROLE,
        driverName: process.env.PUBLISHER_NAME,
        driverTotal: process.env.PUBLISHER_TOTAL,
      })
      .then(() => connHandler("eventDriver"))
      .catch((e) => errorHandler(ERROR.DRIVER100, e));


    eventBus.client.on("end", (e: any) => errorHandler(ERROR.BUS101, e));

    eventBus.client.on("error", (e: any) => errorHandler(ERROR.BUS102, e, true));

    mongoWrapper.client.on("close", (e: any) => errorHandler(ERROR.MONGO101, e));

    mongoWrapper.client.on("error", (e) => errorHandler(ERROR.MONGO102, e));

    eventDriver.on("error", (e: any) => errorHandler(ERROR.DRIVER101, e));

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
