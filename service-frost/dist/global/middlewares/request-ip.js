"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.requestIp = void 0;
const requestIp = (req, res, next) => {
    req.clientIp = req.headers["cf-connecting-ip"] || req.ip;
    next();
};
exports.requestIp = requestIp;
