import { ClientSession } from "mongoose";

export async function transactionRetry(
    session: ClientSession,
    maxRetryCount: number = 50
  ) {
    let count = 0;
    while (session.inTransaction()) {
      if (count >= maxRetryCount) {
        break;
      }
  
      await new Promise((r) => setTimeout(r, 100));
      count++;
    }
  }