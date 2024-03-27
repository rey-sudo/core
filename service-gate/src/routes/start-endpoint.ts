import DB from "../db";
import API from "../api";
import assert from "assert";
import { Request, Response } from "express";
import { requireAuth } from "../utils/required";
import { sellerMiddleware } from "../utils/seller";
import { BadRequestError } from "../errors";

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

    const instanceScheme: instanceScheme = {
      startDefault: {
        sWalletParam: params.seller_pubkeyhash,
        pPriceParam: SLOT.contract_price,
        sCollateralParam: SLOT.contract_collateral,
      },
    };

    const callEndpoint = await API.post(
      `/api/contract/instance/${SLOT.contract_id}/endpoint/Start`,
      instanceScheme
    )
      .then((res) => {
        console.log(res);
      })
      .catch(() => {
        throw new Error("CID_FAILED");
      });

    const getStatus = await API.get(
      `/api/contract/instance/${SLOT.contract_id}/status`
    )
      .then((res) => {
        console.log(res);
      })
      .catch(() => {
        throw new Error("CID_FAILED");
      });

    //  await connection.execute(schemeData, schemeValue);

    await connection.commit();

    res.status(200).send({ success: true });
  } catch (err: any) {
    await connection.rollback();

    throw new BadRequestError(err.message);
  } finally {
    connection.release();
  }
};

export { startEndpointMiddlewares, startEndpointHandler };
