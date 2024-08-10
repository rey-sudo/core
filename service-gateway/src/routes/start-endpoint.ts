import DB from "../db";
import API from "../api";
import assert from "assert";
import { Request, Response } from "express";
import { requireAuth } from "../utils/required";
import { sellerMiddleware } from "../utils/seller";
import { BadRequestError } from "../errors";
import { sleep } from "../utils/sleep";

const ADA_LOVELACE: number = 1000000;

interface InstanceScheme {
  startDefault: {
    sWalletParam: string;
    pPriceParam: number;
    sCollateralParam: number;
  };
}
////////////////////////////////////////////////////MIDDLEWARES

const startEndpointMiddlewares: any = [sellerMiddleware, requireAuth];

////////////////////////////////////////////////////

const startEndpointHandler = async (req: Request, res: Response) => {
  let connection: any = null;

  const params = req.body;

  const SELLER = req.sellerData;

  try {
    connection = await DB.client.getConnection();

    await connection.beginTransaction();

    const [slots] = await connection.execute(
      "SELECT * FROM slots WHERE id = ? AND seller_id = ?",
      [params.slot_id, SELLER.id]
    );

    if (slots.length === 0) {
      throw new Error("NO_SLOT");
    }

    const SLOT = slots[0];

    if (SLOT.actived) {
      throw new Error("IS_ACTIVED");
    }

    ////////////////////////////////////////////////////

    const instanceScheme: InstanceScheme = {
      startDefault: {
        sWalletParam: params.seller_pubkeyhash,
        pPriceParam: SLOT.contract_price * ADA_LOVELACE,
        sCollateralParam: SLOT.contract_collateral * ADA_LOVELACE,
      },
    };

    console.log(instanceScheme);

    //////////////////////////////////////////////

    let contractStatus: any = null;

    await API.post(
      `/api/contract/instance/${SLOT.contract_id}/endpoint/Start`,
      instanceScheme
    )
      .then((res) => assert.ok(res.status === 200))
      .then(() => sleep(2000))
      .then(() => API.get(`/api/contract/instance/${SLOT.contract_id}/status`))
      .then((res) => {
        assert.ok(res.data.cicYieldedExportTxs.length !== 0);
        assert.ok(
          res.data.cicYieldedExportTxs[0].hasOwnProperty("transaction")
        );
        assert.ok(res.data.cicYieldedExportTxs[0].transaction.length !== 0);
        contractStatus = res.data;
      })
      .catch(() => {
        throw new Error("CID_FAILED");
      });

    console.log(contractStatus);

    //////////////////////////////////////////////

    const schemeData = `
      UPDATE slots 
      SET actived = ?,
          seller_pubkeyhash = ?,
          contract_stage = ?,
          contract_0_utx = ?
      WHERE id = ? AND seller_id = ?
      `;

    const schemeValue = [
      true,
      params.seller_pubkeyhash,
      "start",
      contractStatus.cicYieldedExportTxs[0].transaction,
      params.slot_id,
      SELLER.id,
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
  } catch (err: any) {
    await connection.rollback();

    throw new BadRequestError(err.message);
  } finally {
    connection.release();
  }
};

export { startEndpointMiddlewares, startEndpointHandler };
