import { CustomError } from 'ts-custom-error';
export declare enum DataSignErrorCode {
    ProofGeneration = 1,
    AddressNotPK = 2,
    UserDeclined = 3,
    InvalidFormat = 4
}
export declare class DataSignError extends CustomError {
    code: DataSignErrorCode;
    info: string;
    constructor(code: DataSignErrorCode, info: string);
}
//# sourceMappingURL=DataSignError.d.ts.map