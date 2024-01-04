"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.createRoundHandler = exports.createRoundMiddlewares = void 0;
const createRoundMiddlewares = [];
exports.createRoundMiddlewares = createRoundMiddlewares;
const createRoundHandler = async (req, res) => {
    res.status(200).send({});
};
exports.createRoundHandler = createRoundHandler;
