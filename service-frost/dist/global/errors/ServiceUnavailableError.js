"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.ServiceUnavailableError = void 0;
const CustomError_1 = require("./CustomError");
class ServiceUnavailableError extends CustomError_1.CustomError {
    message;
    statusCode = 503;
    constructor(message) {
        super(message);
        this.message = message;
        Object.setPrototypeOf(this, ServiceUnavailableError.prototype);
    }
    serializeErrors() {
        return [{ message: this.message }];
    }
}
exports.ServiceUnavailableError = ServiceUnavailableError;
