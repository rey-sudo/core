import { createClient } from "redis";

class RedisWrapper {
  private _client?: any;

  get client() {
    if (!this._client) {
      throw new Error("Cannot access REDIS client before connecting");
    }

    return this._client;
  }

  connect(options?: any) {
    this._client = createClient(options);

    this.client.on("error", () => null);
    this.client.on("end", () => null);

    return this.client.connect();
  }
}

const eventBus = new RedisWrapper();

const sendEvent = async (clientId: string, type: string) => {
  await eventBus.client.publish(clientId, type);
};

export { RedisWrapper, eventBus, sendEvent };
