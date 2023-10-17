import { BaseDriver, EVENT } from "@alphaicterus/global";
import { MicroStorePublisher } from "../event-bus/publishers/micro-store-pub";

export class EventDriver extends BaseDriver {
  async onMessage(data: any) {
    try {
      const { event } = data.fullDocument;

      switch (event[0]) {
        case EVENT.micro_store:
          const response = await new MicroStorePublisher(
            this.options.driverName,
            this.busClient
          ).publish(this.changeStream.resumeToken, [event[1], event[2]]);

          if (!response) throw new Error("Event publisher error");

          break;
      }
    } catch (err) {
      this.emit("error", err);
    }
  }
}

export const eventDriver = new EventDriver();
