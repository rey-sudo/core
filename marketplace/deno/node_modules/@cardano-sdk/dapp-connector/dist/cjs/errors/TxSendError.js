"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.TxSendError = exports.TxSendErrorCode = void 0;
const ts_custom_error_1 = require("ts-custom-error");
var TxSendErrorCode;
(function (TxSendErrorCode) {
    TxSendErrorCode[TxSendErrorCode["Refused"] = 1] = "Refused";
    TxSendErrorCode[TxSendErrorCode["Failure"] = 2] = "Failure";
})(TxSendErrorCode = exports.TxSendErrorCode || (exports.TxSendErrorCode = {}));
class TxSendError extends ts_custom_error_1.CustomError {
    constructor(code, info) {
        super();
        this.code = code;
        this.info = info;
    }
}
exports.TxSendError = TxSendError;
//# sourceMappingURL=TxSendError.js.map