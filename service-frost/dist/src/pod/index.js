"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.checkPod = exports.errorHandler = exports.checkpoint = void 0;
const logger_1 = require("../utils/logger");
const app_1 = require("../app");
const checkList = {
    ready: false,
};
function checkpoint(processName) {
    logger_1._.info(`${processName} connected`);
    Object.defineProperty(checkList, processName, {
        value: true,
    });
    if (!checkPodList(checkList)) {
        const port = process.env.EXPRESS_PORT || "8000";
        const timeout = process.env.EXPRESS_TIMEOUT || "5000";
        const server = app_1.app.listen(port, () => logger_1._.info(`express server listening in ${port}`));
        server.setTimeout(parseInt(timeout));
    }
}
exports.checkpoint = checkpoint;
function checkPodList(checkList) {
    return Object.values(checkList).includes(false);
}
function checkPod() {
    setTimeout(() => (checkPodList(checkList) ? errorHandler("pod timeout") : false), 120000);
}
exports.checkPod = checkPod;
function errorHandler(msg, err, bypass) {
    logger_1._.error(`[POD-EXIT]:${msg} | ${err}`);
    if (bypass)
        return;
    process.exit(1);
}
exports.errorHandler = errorHandler;
