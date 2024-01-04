"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.errorMiddleware = void 0;
const CustomError_1 = require("../errors/CustomError");
const errorMiddleware = (err, req, res, next) => {
    if (err instanceof CustomError_1.CustomError) {
        return res.status(err.statusCode).send({ errors: err.serializeErrors() });
    }
};
exports.errorMiddleware = errorMiddleware;
