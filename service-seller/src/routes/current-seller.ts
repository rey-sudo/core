import { BadRequestError } from "../../global";
import { Request, Response } from "express";

const getAddressUtxos: any = [];

const getAddressUtxosHandler = async (req: Request, res: Response) => {

  res.status(200).send({});
};

export { getAddressUtxos, getAddressUtxosHandler };
