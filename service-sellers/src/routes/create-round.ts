import { BadRequestError } from "../../global";
import { Request, Response } from "express";

const createRoundMiddlewares: any = [];

const createRoundHandler = async (req: Request, res: Response) => {


  res.status(200).send({ });
};

export { createRoundMiddlewares, createRoundHandler };
