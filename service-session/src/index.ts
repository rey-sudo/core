import DB from "./db";
import listenSlots from "./kafka/slots";
import compression from "compression";
import express from "express";
import http from "http";
import { NotFoundError, errorMiddleware } from "./errors";
import { Server } from "socket.io";
import { _ } from "./utils/pino";
import { authMiddleware } from "./utils/seller";
import cookieSession from "cookie-session";

const catcher = (message?: any, error?: any, bypass?: boolean) => {
  _.error(`EXIT=>${message}-${error}`);

  return bypass || process.exit(1);
};

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

    const app = express();

    app.set("trust proxy", 1);

    const server = http.createServer(app);

    const socketServer = new Server(server);

    app.use(express.json());

    const errorEvents: string[] = [
      "exit",
      "SIGINT",
      "SIGTERM",
      "SIGQUIT",
      "uncaughtException",
      "unhandledRejection",
    ];

    errorEvents.forEach((e: string) => process.on(e, (err) => catcher(err)));

    DB.connect({
      host: "mysql",
      port: 3306,
      user: "marketplace",
      password: "password",
      database: "service_session",
    });

    listenSlots();

    const sockets: any = {};

    socketServer.use(authMiddleware);

    const socketConnectionHandler = (socket: any) => {
      
      console.log(socket.user);

      const userId = generateRandomString(4).toLocaleLowerCase();

      console.log("USER CONNECTED" + userId);

      sockets[userId] = socket;

      sockets[userId].on("join", (orderId: string) => {
        sockets[userId].join(orderId);
        console.log("USER JOINED TO ROOM " + orderId);
      });

      sockets[userId].on("message", (payload: string) => {
        const data = JSON.parse(payload);

        const scheme = {
          user: userId,
          content: data.content,
        };

        socketServer.to(data.room).emit("message", JSON.stringify(scheme));
      });

      sockets[userId].on("disconnect", () => {
        console.log("User disconnected");
      });
    };

    socketServer.on("connection", socketConnectionHandler);

    app.get("/api/session/healthcheck", (req, res) => {
      res.status(200).json({ status: "Test OK" });
    });

    app.all("*", (_req, _res) => {
      throw new NotFoundError();
    });

    app.use(errorMiddleware);

    app.use(compression());

    const PORT = process.env.PORT || 3000;

    server.listen(PORT, () => {
      console.log(`Server is running on port ${PORT}`);
    });
  } catch (err) {
    console.error(err);
  }
};

main();

function generateRandomString(length: number) {
  const characters =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  let result = "";
  const charactersLength = characters.length;
  for (let i = 0; i < length; i++) {
    result += characters.charAt(Math.floor(Math.random() * charactersLength));
  }
  return result;
}
