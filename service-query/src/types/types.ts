const typeDefs = `#graphql

type Product {
  title: String
  author: String
}


type Query {
  product(id: String): [Product]
}
`;

export default typeDefs;
