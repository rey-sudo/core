import { CustomError } from 'ts-custom-error';
export declare enum TxSendErrorCode {
    Refused = 1,
    Failure = 2
}
export declare class TxSendError extends CustomError {
    code: TxSendErrorCode;
    info: string;
    constructor(code: TxSendErrorCode, info: string);
}
//# sourceMappingURL=TxSendError.d.ts.map