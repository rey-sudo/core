"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const blockfrost_js_1 = require("@blockfrost/blockfrost-js");
class BlockfrostWrap {
    _client;
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
    connect(options) {
        this._client = new blockfrost_js_1.BlockFrostAPI(options);
        return this.client;
    }
}
const blockfrost = new BlockfrostWrap();
exports.default = blockfrost;
