import { ApolloServer } from "@apollo/server";
import { expressMiddleware } from "@apollo/server/express4";
import { ApolloServerPluginDrainHttpServer } from "@apollo/server/plugin/drainHttpServer";
import cors from "cors";
import express from "express";
import http from "http";
const typeDefs = `#graphql

  type Product {
    title: String
    author: String
  }

  type ProductPage {
    product: [Product]
  }

  type Query {
    productPage: ProductPage
  }
`;
const product = [
    {
        title: "The Awakening",
        author: "Kate Chopin",
    },
];
const productPage = {
    product,
};
const resolvers = {
    Query: {
        productPage: () => productPage,
    },
};
const app = express();
const httpServer = http.createServer(app);
const server = new ApolloServer({
    typeDefs,
    resolvers,
    plugins: [ApolloServerPluginDrainHttpServer({ httpServer })],
});
await server.start();
app.use("/", cors(), express.json(), expressMiddleware(server, {
    context: async ({ req }) => ({ token: null }),
}));
// Modified server startup
await new Promise((resolve) => httpServer.listen({ port: 4000 }, resolve));
console.log(`🚀 Server ready at http://localhost:4000/`);
