
interface Event {
  streamName: string;
  data: any;
}

export abstract class Publisher<T extends Event> {
  abstract streamName: T["streamName"];
  protected client: any;
  protected driver: string;

  constructor(driver: string, client: any) {
    this.client = client;
    this.driver = driver;
  }
  publish(token: any, data: T["data"]) {
    const buff = Buffer.from(JSON.stringify(data[1]));
    return this.client
      .multi()
      .xAdd(this.streamName, "*", { event_action: data[0], event_payload: buff })
      .set(this.driver, JSON.stringify(token))
      .exec();
  }
}
