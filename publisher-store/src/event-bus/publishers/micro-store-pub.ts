import { EVENT, Publisher } from "@alphaicterus/global";

interface MicroStoreEvent {
  streamName: string;
  data: any;
}

export class MicroStorePublisher extends Publisher<MicroStoreEvent> {
  streamName = EVENT.micro_store;
}
