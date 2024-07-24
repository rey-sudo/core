import DB from "../db";
import API from "../api";
import assert from "assert";
import { Request, Response } from "express";
import { sleep } from "../utils/sleep";
import { userMiddleware } from "../utils/user";
import { _ } from "../utils/pino";

const ADA_LOVELACE: number = 1000000;

interface LockingEndpoint {
  lockingDefault: {
    sWalletParam: string;
    pPriceParam: number;
    sCollateralParam: number;
  };

  bWalletParam: string;
}

function checkUTX(status: any) {
  assert.ok(status.cicYieldedExportTxs.length !== 0);

  assert.ok(status.cicYieldedExportTxs[0].hasOwnProperty("transaction"));

  assert.ok(status.cicYieldedExportTxs[0].transaction.length !== 0);

  return status;
}

/////////////////////////////////////////////////////////

const lockingEndpointMiddlewares: any = [userMiddleware]; //

const lockingEndpointHandler = async (req: Request, res: Response) => {
  const params = req.body;

  const BUYER = req.userData;

  let connection: any = null;

  try {
    connection = await DB.client.getConnection();

    await connection.beginTransaction();

    const [slots] = await connection.execute(
      "SELECT * FROM slots WHERE id = ?",
      [params.slot_id]
    );

    if (slots.length === 0) {
      throw new Error("NOT_SLOT");
    }

    const SLOT = slots[0];

    ////////////////////////////////////////////////////

    const lockingEndpoint: LockingEndpoint = {
      lockingDefault: {
        sWalletParam: SLOT.seller_pubkeyhash,
        pPriceParam: SLOT.contract_price * ADA_LOVELACE,
        sCollateralParam: SLOT.contract_collateral * ADA_LOVELACE,
      },

      bWalletParam: BUYER.pubkeyhash,
    };

    await API.post(
      `/api/contract/instance/${SLOT.contract_id}/endpoint/Locking`,
      lockingEndpoint
    ).catch((err) => {
      console.log(err);
    });

    await sleep(2000);

    await API.get(`/api/contract/instance/${SLOT.contract_id}/status`)
      .then((response) => console.log("MID", response))
      .catch(() => {
        throw new Error("CID_FAILED");
      });

    await sleep(2000);
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
      SET buyer_pubkeyhash = ?,
          contract_1_utx = ?
      WHERE id = ?
      `;

    const schemeValue = [
      BUYER.pubkeyhash,
      contractStatus.cicYieldedExportTxs[0].transaction,
      params.slot_id,
    ];

    console.log(schemeValue);

    await connection.execute(schemeData, schemeValue);

    //////////////////////////////////////////////

    await connection.commit();

    res.status(200).send({
      success: true,
      payload: {
        transaction: contractStatus.cicYieldedExportTxs[0].transaction,
      },
    });
  } catch (err: any) {
    await connection.rollback();

    _.error(err);

    res.status(404).send({
      success: false,
    });
  } finally {
    connection.release();
  }
};

export { lockingEndpointMiddlewares, lockingEndpointHandler };
