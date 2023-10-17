import { EVENT, Listener } from "@alphaicterus/global";
import { _ } from "../../utils/logger";

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

    const { pid } = JSON.parse(event_payload);

    const version = event_action + "-" + pid + "-" + id;

    if (this.history.has(version)) {
      return await this.ack(id);
    }

    ////////////////////////////////////////////////
    
    if (event_action === "order-created") {
      console.log("COMPLETEDEEDEDEDEDE");
    }

    ////////////////////////////////////////////////

    await this.ack(id, version);
  }
}
