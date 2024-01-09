import { BadRequestError, hashPassword } from "../../global";
import { Request, Response } from "express";
import { getSellerId } from "../utils/nano";
import { createToken } from "../utils/token";
import { _ } from "../utils/pino";
import DB from "../db";

const createSellerMiddlewares: any = [];

const createSellerHandler = async (req: Request, res: Response) => {
  let conn = null;

  const params = req.body;

  try {
    conn = await DB.client.getConnection();

    await conn.beginTransaction();

    const token = createToken({
      role : "signup",
      entity: "user",
      email: params.email,
      username: params.nickname,
    });

    const password = await hashPassword(params.password);

    const schemeData = `
    INSERT INTO seller (
      seller_id,
      nickname,
      email,
      password_hash,
      verified,
      country,
      completed_sales,
      uncompleted_sales,
      terms,
      avatar_base,
      avatar_path,
      public_ip,
      schema_v
     ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`;

    const schemeValue = [
      getSellerId(),
      params.nickname,
      params.email,
      password,
      false,
      params.country,
      0,
      0,
      "Terms and conditions: Provide correct data for effective shipping.",
      "https://example.com",
      "/avatar.jpg",
      "192.168.1.1",
      0,
    ];

    const result = await conn.execute(schemeData, schemeValue);

    console.log(result);

    await conn.commit();

    res.status(200).send({});
  } catch (err) {
    await conn.rollback();

    _.error(err);

    throw new BadRequestError("failed");
  } finally {
    conn.release();
  }
};

export { createSellerMiddlewares, createSellerHandler };
