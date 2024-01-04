"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.getAddressUtxosHandler = exports.getAddressUtxos = void 0;
const client_1 = __importDefault(require("../client"));
const getAddressUtxos = [];
exports.getAddressUtxos = getAddressUtxos;
const getAddressUtxosHandler = async (req, res) => {
    const networkInfo = await client_1.default.client.network();
    res.status(200).send(networkInfo);
};
exports.getAddressUtxosHandler = getAddressUtxosHandler;
