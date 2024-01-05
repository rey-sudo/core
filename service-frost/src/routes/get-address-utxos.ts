import { BadRequestError } from "../../global";
import { Request, Response } from "express";
import blockfrost from "../client";

const getAddressUtxos: any = [];

const getAddressUtxosHandler = async (req: Request, res: Response) => {
  const { address } = req.params;

  const networkInfo = await blockfrost.client.addresses(address);

  res.status(200).send(networkInfo);
};

export { getAddressUtxos, getAddressUtxosHandler };

/*

  const methodOptions = {
    batchSize?: number | undefined;
    order?: 'asc' | 'desc';
}

*/
