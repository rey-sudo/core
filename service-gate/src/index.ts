import * as route from "./routes";
import { catcher, check, checkpoint } from "./pod/index";
import { NotFoundError, errorMiddleware } from "./errors";
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

    if (!process.env.SELLER_JWT_KEY) {
      throw new Error("SELLER_JWT_KEY error");
    }

    if (!process.env.TOKEN_EXPIRATION) {
      throw new Error("TOKEN_EXPIRATION error");
    }

    DB.connect({
      host: "10.101.74.192",
      port: 3306,
      user: "marketplace",
      password: "password",
      database: "service_gate",
    });

    const { Kafka } = require("kafkajs");

    const kafka = new Kafka({
      clientId: "service-gate",
      ssl: false,
      enforceRequestTimeout: false,
      brokers: [
        "streaming-kafka-bootstrap:9092",
        "streaming-kafka-bootstrap:9092",
        "streaming-kafka-bootstrap:9092",
      ],
    });

    const serviceProductListener = async () => {
      try {
        const consumer = kafka.consumer({ groupId: "service-gate-group" });

        await consumer.connect();

        await consumer.subscribe({
          topic: "fullfillment.service_product.product",
          fromBeginning: true,
        });

        await consumer.run({
          eachMessage: async ({ topic, partition, message }: any) => {
            console.log({
              topic,
              partition,
            });

            const value = JSON.parse(message.value.toString());
            console.log(value);
          },
        });
      } catch (err) {
        console.error(err);
      }
    };

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
      "/api/product/create-product",

      route.createProductMiddlewares,

      route.createProductHandler
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
