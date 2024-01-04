"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.transactionRetry = void 0;
async function transactionRetry(session, maxRetryCount = 50) {
    let count = 0;
    while (session.inTransaction()) {
        if (count >= maxRetryCount) {
            break;
        }
        await new Promise((r) => setTimeout(r, 100));
        count++;
    }
}
exports.transactionRetry = transactionRetry;
