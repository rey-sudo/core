import { BadRequestError } from "../../global";
import { Request, Response } from "express";
import blockfrost from "../client";


const getAddressUtxos: any = [];

const getAddressUtxosHandler = async (req: Request, res: Response) => {

  const networkInfo = await blockfrost.client.network();

  res.status(200).send(networkInfo);
};

export { getAddressUtxos, getAddressUtxosHandler };
