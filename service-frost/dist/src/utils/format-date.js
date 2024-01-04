"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.formatDate = void 0;
const formatDate = (date) => {
    if (!date)
        return "Unassigned Date";
    const dateValue = new Date(date);
    return dateValue.toUTCString();
};
exports.formatDate = formatDate;
