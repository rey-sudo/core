import DB from "../db";
import API from "../api";
import assert from "assert";
import { Request, Response } from "express";
import { requireAuth } from "../utils/required";
import { sellerMiddleware } from "../utils/seller";
import { BadRequestError } from "../errors";
import { sleep } from "../utils/sleep";
import { sendEvent } from "./get-events";

interface instanceScheme {
  startDefault: {
    sWalletParam: string;
    pPriceParam: number;
    sCollateralParam: number;
  };
}

///////////////////////////////////////////////

const startEndpointMiddlewares: any = [sellerMiddleware, requireAuth];

const startEndpointHandler = async (req: Request, res: Response) => {
  const params = req.body;

  const SELLER = req.sellerData;

  let connection: any = null;

  try {
    connection = await DB.client.getConnection();

    await connection.beginTransaction();

    const [slots] = await connection.execute(
      "SELECT * FROM slots WHERE id = ? AND seller_id = ?",
      [params.slot_id, SELLER.id]
    );

    if (slots.length === 0) {
      throw new Error("NOT_SLOT");
    }

    const SLOT = slots[0];

    //isactivated

    const instanceScheme: instanceScheme = {
      startDefault: {
        sWalletParam: params.seller_pubkeyhash,
        pPriceParam: SLOT.contract_price,
        sCollateralParam: SLOT.contract_collateral,
      },
    };

    const getTransaction = await API.post(
      `/api/contract/instance/${SLOT.contract_id}/endpoint/Start`,
      instanceScheme
    )
      .then((res) => assert.ok(res.status === 200))
      .then(() => sleep(1000))
      .then(() => API.get(`/api/contract/instance/${SLOT.contract_id}/status`))
      .then((res) => {
        console.log(res.data);

        assert.ok(res.data.cicYieldedExportTxs.length !== 0);

        assert.ok(
          res.data.cicYieldedExportTxs[0].hasOwnProperty("transaction")
        );

        assert.ok(res.data.cicYieldedExportTxs[0].transaction.length !== 0);

        return res.data.cicYieldedExportTxs[0].transaction;
      })
      .catch(() => {
        throw new Error("CID_FAILED");
      });

    await connection.commit();

    res.status(200).send({
      success: true,
      payload: {
        transaction: getTransaction,
      },
    });

    sendEvent(SELLER.id, "slot:created");
  } catch (err: any) {
    await connection.rollback();

    throw new BadRequestError(err.message);
  } finally {
    connection.release();
  }
};

export { startEndpointMiddlewares, startEndpointHandler };
