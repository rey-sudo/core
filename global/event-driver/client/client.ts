import { MongoClient } from "mongodb";

export class MongoWrapper {
  private _client?: MongoClient | undefined;

  get client() {
    if (!this._client) {
      throw new Error("Cannot access MONGO client before connecting");
    }

    return this._client;
  }

  connect(url: string, options?: any) {
    this._client = new MongoClient(url, options);
    return this.client.connect();
  }
}