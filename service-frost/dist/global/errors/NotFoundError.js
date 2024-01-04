"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.NotFoundError = void 0;
const CustomError_1 = require("./CustomError");
class NotFoundError extends CustomError_1.CustomError {
    statusCode = 404;
    constructor() {
        super("Route not found");
        Object.setPrototypeOf(this, NotFoundError.prototype);
    }
    serializeErrors() {
        return [{ message: "Route not found" }];
    }
}
exports.NotFoundError = NotFoundError;
