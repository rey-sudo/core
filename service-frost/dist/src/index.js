"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const compression_1 = __importDefault(require("compression"));
const routes_1 = require("./routes");
const index_1 = require("./pod/index");
const global_1 = require("../global");
const app_1 = require("./app");
const main = async () => {
    try {
        /*
        if (!process.env.EXPRESS_PORT) {
          throw new Error("EXPRESS_PORT error");
        }
    
        if (!process.env.EXPRESS_TIMEOUT) {
          throw new Error("EXPRESS_TIMEOUT error");
        }
    
        if (!process.env.CORS_DOMAINS) {
          throw new Error("CORS_DOMAINS error");
        }
    
        */
        (0, index_1.checkpoint)("ready");
        ///////////////////////////////////////////////////
        process.on("exit", (e) => (0, index_1.errorHandler)(e));
        process.on("SIGINT", (e) => (0, index_1.errorHandler)(e));
        process.on("SIGTERM", (e) => (0, index_1.errorHandler)(e));
        process.on("SIGQUIT", (e) => (0, index_1.errorHandler)(e));
        process.on("uncaughtException", (e) => (0, index_1.errorHandler)(e));
        process.on("unhandledRejection", (e) => (0, index_1.errorHandler)(e));
        ////////////////////////////////////////////////////
        app_1.app.post("/api/audits/create-round", routes_1.createRoundMiddlewares, routes_1.createRoundHandler);
        app_1.app.get("/api/audits/get-auditor-information", routes_1.auditorInformationMiddlewares, routes_1.auditorInformationHandler);
        app_1.app.all("*", (_req, _res) => {
            throw new global_1.NotFoundError();
        });
        app_1.app.use(global_1.errorMiddleware);
        app_1.app.use((0, compression_1.default)());
    }
    catch (e) {
        (0, index_1.errorHandler)(e);
    }
    (0, index_1.checkPod)();
};
main();
