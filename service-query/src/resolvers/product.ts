import DB from "../db";

const handleQuery = async (_: any, params: { id: string }) => {
  const connection = await DB.client.getConnection();

  const [products] = await connection.execute(
    `
  SELECT *
  FROM products
  WHERE id = ?;
  `,
    [params.id]
  );

  console.log(products);

  await connection.commit();

  connection.release();

  return products;
};

export const product = {
  Query: {
    product: handleQuery,
  },
};
