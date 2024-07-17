import { ApolloServer } from "@apollo/server";
import { startStandaloneServer } from "@apollo/server/standalone";
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
const server = new ApolloServer({
    typeDefs,
    resolvers,
});
// Passing an ApolloServer instance to the `startStandaloneServer` function:
//  1. creates an Express app
//  2. installs your ApolloServer instance as middleware
//  3. prepares your app to handle incoming requests
const { url } = await startStandaloneServer(server, { listen: { port: 4000 } });
console.log(`🚀 Server listening at: ${url}`);
