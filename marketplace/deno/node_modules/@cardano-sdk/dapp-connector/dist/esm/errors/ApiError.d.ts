import { CustomError } from 'ts-custom-error';
export declare enum APIErrorCode {
    InvalidRequest = -1,
    InternalError = -2,
    Refused = -3
}
export declare class ApiError extends CustomError {
    code: APIErrorCode;
    info: string;
    constructor(code: APIErrorCode, info: string);
}
//# sourceMappingURL=ApiError.d.ts.map