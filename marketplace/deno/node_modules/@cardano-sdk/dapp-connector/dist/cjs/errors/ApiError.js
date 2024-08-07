"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.ApiError = exports.APIErrorCode = void 0;
const ts_custom_error_1 = require("ts-custom-error");
var APIErrorCode;
(function (APIErrorCode) {
    APIErrorCode[APIErrorCode["InvalidRequest"] = -1] = "InvalidRequest";
    APIErrorCode[APIErrorCode["InternalError"] = -2] = "InternalError";
    APIErrorCode[APIErrorCode["Refused"] = -3] = "Refused";
})(APIErrorCode = exports.APIErrorCode || (exports.APIErrorCode = {}));
class ApiError extends ts_custom_error_1.CustomError {
    constructor(code, info) {
        super();
        this.code = code;
        this.info = info;
    }
}
exports.ApiError = ApiError;
//# sourceMappingURL=ApiError.js.map