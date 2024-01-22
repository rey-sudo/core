import DB from "../db";
import kafka from "./client";
import { _ } from "../utils/pino";

const TOPIC_NAME = "fullfillment.service_product.product";
const CONSUMER_GROUP = "service-gate-group";

const serviceProductListener = async () => {
  const consumer = kafka.consumer({ groupId: CONSUMER_GROUP });

  await consumer
    .connect()
    .then(() =>
      consumer.subscribe({
        topic: TOPIC_NAME,
        fromBeginning: true,
      })
    )
    .then(() =>
      consumer.run({
        eachMessage: async ({ topic, partition, message }: any) => {
          if (!message.value) return;

          const value = JSON.parse(message.value.toString());

          const payload = value.payload;

          console.log(payload);

          if (payload.op === "c") {
            await handleCreate(payload);
          }

          if (payload.op === "u") {
            await handleUpdate(payload);
          }

          if (payload.op === "d") {
            await handleDelete(payload);
          }
        },
      })
    )
    .catch((err: any) => _.error(err));
};

const handleCreate = async (payload_: any) => {
  const payload = payload_.after;

  let connection = null;

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

    if (rows.length !== 0) {
      throw new Error("ROW_DUPLICATION");
    }

    const [created] = await connection.execute(schemeData, schemeValue);

    if (created.affectedRows === 0) {
      throw new Error("NO_AFFECTED");
    }

    await connection.commit();
  } catch (err) {
    await connection.rollback().then(() => _.error(err));
  } finally {
    connection.release();
  }
};

const handleUpdate = async (payload_: any) => {
  const payload = payload_.after;

  let connection = null;

  try {
    connection = await DB.client.getConnection();

    await connection.beginTransaction();

    const schemeData = `
    UPDATE product 
    SET product_id = ?,
        seller_id = ?,
        title = ?,
        category = ?,
        price = ?,
        collateral = ?,
        stock = ?,
        slots = ?,
        note = ?,
        keywords = ?,
        theme = ?,
        terms = ?,
        country = ?,
        moderated = ?,        
        image_base = ?,
        image_path = ?,
        created_at = ?,
        schema_t = ?,
        schema_v = ?
    WHERE product_id = ? AND schema_v = ?`;

    const schemeValue = [
      ...Object.values(payload),
      payload.product_id,
      payload.schema_v - 1,
    ];

    const [updated] = await connection.execute(schemeData, schemeValue);

    if (updated.affectedRows === 0) {
      throw new Error("NO_AFFECTED");
    }

    await connection.commit();
  } catch (err) {
    await connection.rollback().then(() => _.error(err));
  } finally {
    connection.release();
  }
};

const handleDelete = async (payload_: any) => {
  const payload = payload_.before;

  let connection = null;

  try {
    connection = await DB.client.getConnection();

    await connection.beginTransaction();

    const [deleted] = await connection.execute(
      "DELETE FROM product WHERE product_id = ? AND schema_v = ?",
      [payload.product_id, payload.schema_v]
    );

    if (deleted.affectedRows === 0) {
      throw new Error("NO_AFFECTED");
    }

    await connection.commit();
  } catch (err) {
    await connection.rollback().then(() => _.error(err));
  } finally {
    connection.release();
  }
};

export default serviceProductListener;
