import { BlockFrostAPI } from "@blockfrost/blockfrost-js";

class BlockfrostWrap {
  private _client?: any;

  get client() {
    if (!this._client) {
      throw new Error("Cannot access the client before connecting");
    }

    return this._client;
  }

  get network() {
    if (!this._client) {
      throw new Error("Cannot access the client before connecting");
    }
    
    return this.client.network();
  }

  connect(options?: any): BlockFrostAPI {
    this._client = new BlockFrostAPI(options);

    return this.client;
  }
}

const blockfrost = new BlockfrostWrap();

export default blockfrost;
