import { BadRequestError } from "../../global";
import { Request, Response } from "express";


const auditorInformationMiddlewares: any = [];

const auditorInformationHandler = async (req: Request, res: Response) => {

  res.status(200).send({});
};

export { auditorInformationMiddlewares, auditorInformationHandler };
