"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.DataSignError = exports.DataSignErrorCode = void 0;
const ts_custom_error_1 = require("ts-custom-error");
var DataSignErrorCode;
(function (DataSignErrorCode) {
    DataSignErrorCode[DataSignErrorCode["ProofGeneration"] = 1] = "ProofGeneration";
    DataSignErrorCode[DataSignErrorCode["AddressNotPK"] = 2] = "AddressNotPK";
    DataSignErrorCode[DataSignErrorCode["UserDeclined"] = 3] = "UserDeclined";
    DataSignErrorCode[DataSignErrorCode["InvalidFormat"] = 4] = "InvalidFormat";
})(DataSignErrorCode = exports.DataSignErrorCode || (exports.DataSignErrorCode = {}));
class DataSignError extends ts_custom_error_1.CustomError {
    constructor(code, info) {
        super();
        this.code = code;
        this.info = info;
    }
}
exports.DataSignError = DataSignError;
//# sourceMappingURL=DataSignError.js.map