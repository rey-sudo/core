import { CustomError } from "./CustomError";

export class TooManyRequests extends CustomError {
  statusCode = 429;

  constructor(public message: string) {
    super(message);

    Object.setPrototypeOf(this, TooManyRequests.prototype);
  }

  serializeErrors() {
    return [{ message: this.message }];
  }
}
