import DB from "../db";
import API from "../api";
import assert from "assert";
import { Request, Response } from "express";
import { BadRequestError } from "../errors";
import { sleep } from "../utils/sleep";
import { userMiddleware } from "../utils/user";

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
  console.log(status);

  assert.ok(status.cicYieldedExportTxs.length !== 0);

  assert.ok(status.cicYieldedExportTxs[1].hasOwnProperty("transaction"));

  assert.ok(status.cicYieldedExportTxs[1].transaction.length !== 0);

  return status;
}

/////////////////////////////////////////////////////////

const lockingEndpointMiddlewares: any = [userMiddleware];

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

    console.log(lockingEndpoint);

    await API.post(
      `/api/contract/instance/${SLOT.contract_id}/endpoint/Locking`,
      lockingEndpoint
    )
      .then((res) => {
        console.log(res);
        assert.ok(res.status === 200);
      })
      .catch((err) => {
        console.log(err);
        throw new Error("CID_FAILED");
      });

    await sleep(1000);
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
          contract_utx_1 = ?
      WHERE id = ?
      `;

    const schemeValue = [
      "locking",
      contractStatus,
      contractStatus.cicYieldedExportTxs[1].transaction,
      params.slot_id,
    ];

    await connection.execute(schemeData, schemeValue);

    //////////////////////////////////////////////

    await connection.commit();

    res.status(200).send({
      success: true,
      payload: {
        transaction: contractStatus.cicYieldedExportTxs[1].transaction,
      },
    });
  } catch (err: any) {
    await connection.rollback();

    res.status(200).send({
      success: false,
    });
  } finally {
    connection.release();
  }
};

export { lockingEndpointMiddlewares, lockingEndpointHandler };
