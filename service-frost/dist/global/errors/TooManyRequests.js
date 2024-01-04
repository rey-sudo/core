"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.TooManyRequests = void 0;
const CustomError_1 = require("./CustomError");
class TooManyRequests extends CustomError_1.CustomError {
    message;
    statusCode = 429;
    constructor(message) {
        super(message);
        this.message = message;
        Object.setPrototypeOf(this, TooManyRequests.prototype);
    }
    serializeErrors() {
        return [{ message: this.message }];
    }
}
exports.TooManyRequests = TooManyRequests;
