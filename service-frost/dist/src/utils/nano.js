"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.largePid = exports.customPid = void 0;
const nanoid_1 = require("nanoid");
const customPid = (0, nanoid_1.customAlphabet)("0123456789ABCDEFGHJKLMNOPQRTUVWXY", 12);
exports.customPid = customPid;
const largePid = (0, nanoid_1.customAlphabet)("0123456789ABCDEFGHJKLMNOPQRTUVWXYZ", 20);
exports.largePid = largePid;
