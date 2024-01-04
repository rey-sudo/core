import { commandOptions } from "redis";
const _ = require("pino")();
const EventEmitter = require("events").EventEmitter;

type EventEmitter = typeof EventEmitter;

interface Event {
  streamName: string;
  data: any;
}

export abstract class Listener<T extends Event> {
  status: EventEmitter;
  history: Set<string>;
  abstract onMessage(data: T["data"]): void;
  abstract streamName: T["streamName"];
  abstract consumerName: string;
  abstract consumerGroup: string;
  protected client: any;

  constructor(client: any) {
    this.client = client;
    this.status = new EventEmitter();
    this.status.on("error", () => null);
    this.history = new Set();
  }

  getPending() {
    return this.client.xReadGroup(
      commandOptions({
        isolated: true,
      }),
      this.consumerGroup,
      this.consumerName,
      [
        {
          key: this.streamName,
          id: "0",
        },
      ]
    );
  }

  deletePending() {
    return this.client.xPendingRange(
      this.streamName,
      this.consumerGroup,
      "-",
      "+",
      100
    );
  }

  async runListener() {
    let loop = true;
    while (loop) {
      await this.client
        .xReadGroup(
          commandOptions({
            isolated: true,
          }),
          this.consumerGroup,
          this.consumerName,
          [
            {
              key: this.streamName,
              id: ">",
            },
          ],
          {
            COUNT: 200,
            BLOCK: 2000,
          }
        )
        .then(async (stream: any) => {
          if (stream)
            stream[0].messages.forEach((element: any) =>
              this.onMessage(element)
            );
          else {
            await this.getPending().then((pending: any) => {
              if (pending)
                pending[0].messages.forEach((element: any) =>
                  this.onMessage(element)
                );
            });

            await this.deletePending().then((range: any) => {
              range.forEach(async (e: any) => {
                if (e.deliveriesCounter > 100) {
                  await this.acknowledge(e.id);
                }
              });
            });
          }
        })
        .catch((e: any) => this.status.emit("error", e) && (loop = false));
    }
  }

  async setupGroup() {
    while (true) {
      try {
        await this.client.xGroupCreate(
          this.streamName,
          this.consumerGroup,
          "0",
          {
            MKSTREAM: true,
          }
        );
        _.info("consumer group created");
      } catch (e) {
        _.error("consumer group already exists");
        break;
      }
    }
  }

  acknowledge(id: string) {
    return this.client.xAck(this.streamName, this.consumerGroup, id);
  }

  ack(id: string, version?: string) {
    if (version) {
      this.history.add(version);
    }

    if (this.history.size > 10000) this.history.clear();
    
    return this.acknowledge(id);
  }

  listen() {
    this.setupGroup();
    this.runListener();
    return new Promise((resolve) => resolve(this.status));
  }
}
