import DB from "../db";
import API from "../api";
import assert from "assert";
import { Request, Response } from "express";
import { BadRequestError } from "../errors";
import { sendEvent } from "./get-events";

const ADA_LOVELACE: number = 1000000;

interface instanceScheme {
  lockingDefault: {
    sWalletParam: string;
    pPriceParam: number;
    sCollateralParam: number;
  };

  bWalletParam: string;
}

function checkUTX(status: any) {
  console.log(status);

  assert.ok(status.cicYieldedExportTxs.length !== 0);

  assert.ok(status.cicYieldedExportTxs[0].hasOwnProperty("transaction"));

  assert.ok(status.cicYieldedExportTxs[0].transaction.length !== 0);

  return status;
}

/////////////////////////////////////////////////////////

const lockingEndpointMiddlewares: any = [];

const lockingEndpointHandler = async (req: Request, res: Response) => {
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

    ////////////////////////////////////////////////////

    const instanceScheme: instanceScheme = {
      lockingDefault: {
        sWalletParam: SLOT.seller_pubkeyhash,
        pPriceParam: SLOT.contract_price * ADA_LOVELACE,
        sCollateralParam: SLOT.contract_collateral * ADA_LOVELACE,
      },

      bWalletParam: params.buyer_pubkeyhash,
    };

    await API.post(
      `/api/contract/instance/${SLOT.contract_id}/endpoint/Locking`,
      instanceScheme
    )
      .then((res) => assert.ok(res.status === 200))
      .catch(() => {
        throw new Error("CID_FAILED");
      });

    ////////////////////////////////////////////////////

    const contractStatus = await API.get(
      `/api/contract/instance/${SLOT.contract_id}/status`
    )
      .then((response) => checkUTX(response.data))
      .catch(() => {
        throw new Error("CID_FAILED");
      });

    //////////////////////////////////////////////

    const schemeData = `
      UPDATE slots 
      SET contract_stage = ?,
          contract_status_1 = ?,
          contract_utx_1 = ?
      WHERE id = ? AND seller_id = ?
      `;

    const schemeValue = [
      "locking",
      contractStatus,
      contractStatus.cicYieldedExportTxs[0].transaction,
      params.slot_id,
      SELLER.id
    ];

    await connection.execute(schemeData, schemeValue);

    //////////////////////////////////////////////

    await connection.commit();

    res.status(200).send({
      success: true,
      payload: {
        transaction: contractStatus.cicYieldedExportTxs[0].transaction,
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

export { lockingEndpointMiddlewares, lockingEndpointHandler };
