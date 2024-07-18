const typeDefs = `#graphql

type Product {
  title: String
  author: String
}


type Query {
  product: [Product]
}
`;

export default typeDefs;
