import { CustomError } from 'ts-custom-error';
export declare class PaginateError extends CustomError {
    maxSize: number;
    constructor(maxSize: number, message: string);
}
//# sourceMappingURL=PaginateError.d.ts.map