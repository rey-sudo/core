import { EVENT, Publisher } from "@alphaicterus/global";

interface AuditActionEvent {
  streamName: string;
  data: any;
}

export class AuditActionPublisher extends Publisher<AuditActionEvent> {
  streamName = EVENT.service_audits;
}
