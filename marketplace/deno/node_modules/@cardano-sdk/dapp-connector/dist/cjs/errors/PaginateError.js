"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.PaginateError = void 0;
const ts_custom_error_1 = require("ts-custom-error");
class PaginateError extends ts_custom_error_1.CustomError {
    constructor(maxSize, message) {
        super();
        this.maxSize = maxSize;
        this.message = message;
    }
}
exports.PaginateError = PaginateError;
//# sourceMappingURL=PaginateError.js.map