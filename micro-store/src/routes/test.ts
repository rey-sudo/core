import { BadRequestError, expressBodyValidator } from "@alphaicterus/global";
import { Request, Response } from "express";
import { Report } from "../models/report";
import { GET_ALL_REPORTS } from "../utils/body-validator";

const testMiddlewares = [GET_ALL_REPORTS, expressBodyValidator];

/**USER | HANDLER | GET
 *
 * 
 *
 */
const testHandler = async (req: Request, res: Response) => {
  const { project_name } = req.params;

  const findReports = await Report.find({ "project.name": project_name })
    .select({
      _id: 0,
      pid: 1,
      round: 1,
      group_name: 1,
      total_score: 1,
      total_percentage: 1,
      createdAt: 1,
      updatedAt: 1,
    })
    .lean();

  if (!findReports.length) {
    throw new BadRequestError("NO_REPORTS");
  }

  res.status(200).send({ test:  "test"});
};

export { testMiddlewares, testHandler };
