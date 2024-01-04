import { BadRequestError } from "../../global;
import { Request, Response } from "express";

import { currentAuditor } from "../utils/current-auditor";
import { auditorRequiredAuth } from "../utils/auditor-required-auth";

const auditorInformationMiddlewares = [currentAuditor, auditorRequiredAuth];

const auditorInformationHandler = async (req: Request, res: Response) => {

  res.status(200).send({});
};

export { auditorInformationMiddlewares, auditorInformationHandler };
