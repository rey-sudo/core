import cors from "cors";
import express from "express";
import http from "http";
import DB from "./db";
import listenProducts from "./kafka/products";
import typeDefs from "./types/types";
import { ApolloServer } from "@apollo/server";
import { expressMiddleware } from "@apollo/server/express4";
import { ApolloServerPluginDrainHttpServer } from "@apollo/server/plugin/drainHttpServer";
import { product } from "./resolvers/product";

const resolvers = {
  Query: {
    ...product.Query,
  },
};

DB.connect({
  host: "mysql",
  port: 3306,
  user: "marketplace",
  password: "password",
  database: "service_query",
});

listenProducts();

const app = express();

const httpServer = http.createServer(app);

const server = new ApolloServer({
  typeDefs,
  resolvers,
  plugins: [ApolloServerPluginDrainHttpServer({ httpServer })],
});

const main = async () => {
  await server.start();

  app.use(
    "/api/query",
    cors<cors.CorsRequest>(),
    express.json(),
    expressMiddleware(server, {
      context: async ({ req }) => ({ token: null }),
    })
  );

  await new Promise<void>((resolve) =>
    httpServer.listen({ port: 4000 }, resolve)
  );

  console.log(`🚀 Server ready at http://localhost:4000/`);
};

main();
