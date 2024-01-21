import DB from "../db";
import kafka from "./client";
import { _ } from "../utils/pino";

export const serviceProductListener = async () => {
  try {
    const consumer = kafka.consumer({ groupId: "service-gate-group" });

    await consumer.connect();

    await consumer.subscribe({
      topic: "fullfillment.service_product.product",
      fromBeginning: true,
    });

    await consumer.run({
      eachMessage: async ({ topic, partition, message }: any) => {
        const value = JSON.parse(message.value.toString());

        const payload = value.payload.after;

        let connection = null;

        console.log({
          topic,
          partition,
          value,
          payload,
        });

        try {
          connection = await DB.client.getConnection();

          await connection.beginTransaction();

          const schemeData = `
            INSERT INTO product (
              product_id,
              seller_id,
              title,
              category,
              price,
              collateral,
              stock,
              slots,
              note,
              keywords,
              theme,
              terms,
              country,
              moderated,
              image_base,
              image_path,
              created_at,
              schema_t,
              schema_v
             ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`;

          const schemeValue = Object.values(payload);

          const [rows] = await connection.execute(
            "SELECT * FROM product WHERE product_id = ?",
            [payload.product_id]
          );

          if (rows.length === 0) {
            await connection.execute(schemeData, schemeValue);
            return await connection.commit();
          }

          if (rows[0].schema_v === payload.schema_v - 1) {
            console.log("v", rows[0].schema_v);
            await connection.execute(schemeData, schemeValue);
            return await connection.commit();
          }
        } catch (err) {
          await connection.rollback();
          _.error(err);
        } finally {
          connection.release();
        }
      },
    });
  } catch (err) {
    console.error(err);
  }
};
