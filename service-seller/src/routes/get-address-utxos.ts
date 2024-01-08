import { BadRequestError } from "../../global";
import { Request, Response } from "express";
import blockfrost from "../client";
import DB from "../db";

const getAddressUtxos: any = [];

const getAddressUtxosHandler = async (req: Request, res: Response) => {

  const xs = await DB`
  insert into users (
    name, age
  ) values (
    'Murray', 68
  )

  returning *
`;
  res.status(200).send(xs);
};

export { getAddressUtxos, getAddressUtxosHandler };

/*

  const methodOptions = {
    batchSize?: number | undefined;
    order?: 'asc' | 'desc';
}

*/
