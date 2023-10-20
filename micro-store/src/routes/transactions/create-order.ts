import mongoose from "mongoose";
import { EVENT, Pub } from "@alphaicterus/global";
import { Order, OrderAttrs, OrderDocument } from "../../models";
import { _ } from "../../utils/logger";


export const createOrder = async (params: OrderAttrs) => {
  let response: any = null;
  let session: any = null;
  await mongoose
    .startSession()
    .then((_session) => {
      session = _session;
      return session.withTransaction(async () => {
        const orderCreated = await Order.build(params, session);

        if (!orderCreated[0]) {
          throw new Error("CREATE_ORDER_ERROR");
        }

        const createEvent = await Pub.build(
          {
            event: [
              EVENT.micro_store,
              "order-created",
              orderCreated[0],
            ],
          },
          session
        );

        if (!createEvent[0]) {
          throw new Error("CREATE_EVENT_ERROR");
        }

        response = orderCreated[0];
      });
    })
    .catch((e) => _.error(e))
    .finally(() => session.endSession());

  return response ? response as OrderDocument : false;
};
