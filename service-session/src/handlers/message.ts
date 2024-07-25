import { socketServer } from "..";
import DB from "../db";
import { sessionRedis } from "../db/session";

export const messageHandler = async (payload: string, AGENT: any) => {
  const data = JSON.parse(payload);

  if (AGENT.role === "SELLER") {
    let connection = null;

    try {
      connection = await DB.client.getConnection();

      const [slots] = await connection.execute(
        "SELECT seller_id FROM slots WHERE id = ?",
        [data.room]
      );

      if (slots.length === 0) {
        return;
      }

      const SLOT = slots[0];

      if (SLOT.seller_id === AGENT.id) {
        const scheme = {
          user: AGENT.id,
          role: AGENT.role,
          content: data.content,
        };

        const message = JSON.stringify(scheme);

        sessionRedis.client.rpush(
          data.room,
          message,
          (err: any, reply: any) => {
            if (err) {
              console.error("Error saving message:", err);
            } else {
              console.log(`Message saved to room ${data.room}: ${message}`);
            }
          }
        );

        socketServer.to(data.room).emit("message", message);
      }
    } catch (err) {
      await connection.rollback();
    } finally {
      connection.release();
    }
  }

  if (AGENT.role === "USER") {
    let connection = null;

    try {
      connection = await DB.client.getConnection();

      const [slots] = await connection.execute(
        "SELECT buyer_pubkeyhash FROM slots WHERE id = ?",
        [data.room]
      );

      if (slots.length === 0) {
        return;
      }

      const SLOT = slots[0];

      if (SLOT.buyer_pubkeyhash === AGENT.pubkeyhash) {
        const scheme = {
          user: AGENT.id,
          role: AGENT.role,
          content: data.content,
        };

        const message = JSON.stringify(scheme);

        sessionRedis.client.rpush(
          data.room,
          message,
          (err: any, reply: any) => {
            if (err) {
              console.error("Error saving message:", err);
            } else {
              console.log(`Message saved to room ${data.room}: ${message}`);
            }
          }
        );

        socketServer.to(data.room).emit("message", message);
      }
    } catch (err) {
      await connection.rollback();
    } finally {
      connection.release();
    }
  }
};
