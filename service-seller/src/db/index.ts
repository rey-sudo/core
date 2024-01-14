import postgres from "postgres";

class DatabaseWrap {
  private _client?: any;

  get client() {
    if (!this._client) {
      throw new Error("Cannot access the client before connecting");
    }

    return this._client;
  }

  connect(options: any): postgres.Sql {
    this._client = postgres(options);
    return this.client;
  }
}

const DB = new DatabaseWrap();

export default DB;
