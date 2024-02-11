import { clients } from "../routes/get-events";

function getStockStatus(stock: number): string {
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

function sendEvent(clientId: string, type: string, payload?: any) {
  if (clients.hasOwnProperty(clientId)) {
    const scheme = {
      type: type,
      client: clientId,
      payload: payload,
    };

    clients[clientId].write(`data: ${JSON.stringify(scheme)}\n\n`);
  }
}

export { getStockStatus, sendEvent };
