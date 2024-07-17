import { createPool } from "mysql2/promise";
class DatabaseWrap {
    get client() {
        if (!this._client) {
            throw new Error("Cannot access the client before connecting");
        }
        return this._client;
    }
    connect(options) {
        this._client = createPool(options);
        return this.client;
    }
}
const DB = new DatabaseWrap();
export default DB;
