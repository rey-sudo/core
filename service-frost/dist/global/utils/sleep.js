"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.BLOCK = void 0;
function BLOCK(timeInMs) {
    timeInMs =
        typeof timeInMs === "string" ? (timeInMs = parseInt(timeInMs)) : timeInMs;
    return new Promise((resolve) => setTimeout(() => resolve(false), timeInMs));
}
exports.BLOCK = BLOCK;
