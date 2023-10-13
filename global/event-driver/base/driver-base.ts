import { MongoClient } from "mongodb";
import { BLOCK } from "../../utils/sleep";
const _ = require("pino")();

let EventEmitter = require("events").EventEmitter;

export interface EventDriverOptions {
  driverName: string;
  driverRole: string;
  driverTotal: string;
}

export abstract class BaseDriver extends EventEmitter {
  abstract onMessage(data: any): void;

  constructor() {
    super();
  }

  async connect(
    dbClient: MongoClient,
    busClient: any,
    options: EventDriverOptions
  ): Promise<void> {
    this.dbClient = dbClient;
    this.busClient = busClient;
    this.options = options;

    return new Promise<void>((resolve, reject) => {
      this.main((err: any) => {
        if (err) {
          return reject(err);
        }
        resolve();
      });
    });
  }

  async subscribe(
    busClient: any,
    driverRole: EventDriverOptions["driverRole"]
  ): Promise<void> {
    return new Promise<void>((resolve, reject) => {
      this.pilot(busClient, driverRole, (err: any) => {
        if (err) {
          return reject(err);
        }
        resolve();
      });
    });
  }

  async main(callback: any) {
    try {
      const pubCollection = this.dbClient.db().collection("pubs");

      const regexFilter = getDriverWork(
        this.options.driverName,
        this.options.driverTotal
      );

      const driverKey = await this.busClient.get(this.options.driverName);

      const resumeToken = driverKey ? JSON.parse(driverKey) : undefined;

      const pipeFilter = [
        {
          $addFields: { "documentKey._id": { $toString: "$documentKey._id" } },
        },
        { $match: { "documentKey._id": regexFilter } },
      ];

      this.changeStream = pubCollection.watch(pipeFilter, {
        resumeAfter: resumeToken,
        readConcern: "majority",
        readPreference: "primary",
      });

      this.setup();

      this.listen();

      this.liveness();

      callback();
    } catch (err) {
      callback(err);
    }
  }

  async setup() {
    try {
      const driverTotal = await this.busClient.get(this.options.driverRole);

      if (!driverTotal) {
        await this.busClient
          .multi()
          .set(this.options.driverRole, this.options.driverTotal)
          .exec();

        return;
      }

      if (driverTotal === this.options.driverTotal) {
        return;
      }

      const deleteKeys = this.busClient.multi();

      for (let i = 0; i < parseInt(driverTotal); i++) {
        deleteKeys.del(`${this.options.driverRole}-${i}`);
      }

      await deleteKeys.exec();
    } catch (err) {
      this.emit("error", err);
    }
  }

  listen() {
    this.changeStream.on("change", (data: any) => {
      this.onMessage(data);
    });
  }

  async liveness() {
    try {
      const publisher = this.busClient.duplicate();

      await publisher.connect();

      while (true) {
        await publisher.publish(
          this.options.driverRole,
          `${this.options.driverName}/${this.options.driverTotal}`
        );
        await BLOCK(5000);
      }
      
    } catch (err) {
      this.emit("error", err);
    }
  }

  async pilot(
    busClient: any,
    driverRole: EventDriverOptions["driverRole"],
    callback: any
  ) {
    try {
      if (!busClient || !driverRole) {
        throw new Error("busClient or driverRole undefined");
      }
      const slaves = new Set();

      const subscriber = busClient.duplicate();

      await subscriber.connect();

      await subscriber.subscribe(driverRole, (message: any) => {
        const [driverName, driverTotal] = message.split("/");

        slaves.add(driverName);

        if (slaves.size > 0) {
          _.info(`${driverRole} - slaves: ${slaves.size}/${driverTotal}`);
          callback();
        } else {
          _.warn(`slaves: ${slaves.size}/${driverTotal}`);
        }
      });

      while (true) {
        slaves.clear();
        await BLOCK(10000);
      }
    } catch (err) {
      callback(err);
    }
  }
}

const baseHex = [
  "0",
  "1",
  "2",
  "3",
  "4",
  "5",
  "6",
  "7",
  "8",
  "9",
  "a",
  "b",
  "c",
  "d",
  "e",
  "f",
];

function hexToChuncks(replicas: any) {
  let result = [];
  for (let i = replicas; i > 0; i--) {
    result.push(baseHex.splice(0, Math.ceil(baseHex.length / i)));
  }
  return result;
}

function chunkToRegex(array: any[]) {
  const result = array.map((sub) => {
    return RegExp(`[${sub[0]}-${sub[sub.length - 1]}]$`);
  });
  return result;
}

function getReplicaNumber(name: string) {
  const chunks = name.split("-");
  const number = parseInt(chunks[chunks.length - 1]);
  return number;
}

function getDriverWork(n: string, replicas: string) {
  const regex = chunkToRegex(hexToChuncks(replicas));

  return regex[getReplicaNumber(n)];
}
