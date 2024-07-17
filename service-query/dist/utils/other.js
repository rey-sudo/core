function getStockStatus(stock) {
    if (stock < 1) {
        return "out";
    }
    if (stock < 10) {
        return "low";
    }
    if (stock < 20) {
        return "stock";
    }
    return "stock";
}
function sleep(timeInMs) {
    timeInMs =
        typeof timeInMs === "string" ? (timeInMs = parseInt(timeInMs)) : timeInMs;
    return new Promise((resolve) => setTimeout(() => resolve(false), timeInMs));
}
export { getStockStatus, sleep };
