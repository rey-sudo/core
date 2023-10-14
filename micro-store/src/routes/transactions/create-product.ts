import mongoose from "mongoose";
import { EVENT, Pub } from "@alphaicterus/global";
import { Product, ProductAttrs } from "../../models";
import { _ } from "../../utils/logger";


export const createProduct = async (params: ProductAttrs) => {
  let response: any = null;
  let session: any = null;
  await mongoose
    .startSession()
    .then((_session) => {
      session = _session;
      return session.withTransaction(async () => {
        const productCreated = await Product.build(params, session);

        if (!productCreated[0]) {
          throw new Error("CREATE_PRODUCT_ERROR");
        }

        const createEvent = await Pub.build(
          {
            event: [
              EVENT.service_email,
              "product-created",
              productCreated[0],
            ],
          },
          session
        );

        if (!createEvent[0]) {
          throw new Error("CREATE_EVENT_ERROR");
        }

        response = productCreated[0];
      });
    })
    .catch((e) => _.error(e))
    .finally(() => session.endSession());

  return response ? { success: true } : { success: false };
};
