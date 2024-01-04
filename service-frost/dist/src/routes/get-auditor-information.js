"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.auditorInformationHandler = exports.auditorInformationMiddlewares = void 0;
const auditorInformationMiddlewares = [];
exports.auditorInformationMiddlewares = auditorInformationMiddlewares;
const auditorInformationHandler = async (req, res) => {
    res.status(200).send({});
};
exports.auditorInformationHandler = auditorInformationHandler;
