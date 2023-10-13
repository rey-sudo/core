import { BaseDriver, EVENT } from "@alphaicterus/global";
import { AuditActionPublisher } from "../event-bus/publishers/service-audits-publisher";

export class EventDriver extends BaseDriver {
  async onMessage(data: any) {
    try {
      const { event } = data.fullDocument;

      switch (event[0]) {
        case EVENT.service_audits:
          const response = await new AuditActionPublisher(
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
