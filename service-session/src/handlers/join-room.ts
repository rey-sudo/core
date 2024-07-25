import { socketServer, sockets } from "..";
import DB from "../db";

export const joinRoomHandler = async (payload: string, AGENT: any) => {
  const data = JSON.parse(payload);

  if (AGENT.role === "SELLER") {
    let connection = null;

    try {
      connection = await DB.client.getConnection();

      const [slots] = await connection.execute(
        "SELECT * FROM slots WHERE id = ?",
        [data.room]
      );

      if (slots.length === 0) {
        return;
      }

      const SLOT = slots[0];

      if (SLOT.seller_id === AGENT.id) {
        sockets[AGENT.id].join(data.room);

        console.log("USER JOINED TO ROOM " + data.room);

        socketServer.to(data.room).emit("message", "Seller connected");
      }
    } catch (err) {
      await connection.rollback();
    } finally {
      connection.release();
    }
    
  }
};
