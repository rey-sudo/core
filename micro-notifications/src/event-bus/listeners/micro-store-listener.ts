import { EVENT, Listener } from "@alphaicterus/global";
import { _ } from "../../utils/logger";
import { BOT_1_CHATID, bot } from "../../telegram";

interface MicroStoreEvent {
  streamName: string;
  data: any;
}

export class MicroStoreListener extends Listener<MicroStoreEvent> {
  streamName = EVENT.micro_store;
  consumerName = "consumer-1";
  consumerGroup = "notifications-group";

  async onMessage(data: MicroStoreEvent["data"]) {
    _.info(data);

    const {
      id,
      message: { event_action, event_payload },
    } = data;

    const payload = JSON.parse(event_payload);

    const version = event_action + "-" + payload.pid + "-" + id;

    if (this.history.has(version)) {
      return await this.ack(id);
    }

    ////////////////////////////////////////////////
    
    if (event_action === "order-created") {
      console.log("COMPLETEDEEDEDEDEDE", BOT_1_CHATID);

      if(BOT_1_CHATID){
        await bot.telegram.sendMessage(BOT_1_CHATID, event_payload)
      }

    }

    ////////////////////////////////////////////////

    await this.ack(id, version);
  }
}
