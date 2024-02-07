import { hashPassword } from "../utils/password";
import { BadRequestError } from "../errors";
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
      role: "create-seller",
      entity: "seller",
      email: params.email,
      username: params.username,
    });

    const password = await hashPassword(params.password);

    const schemeData = `
    INSERT INTO seller (
      seller_id,
      username,
      email,
      password_hash,
      verified,
      country,
      completed_sales,
      uncompleted_sales,
      trade_terms,
      terms_accepted,
      avatar_base,
      avatar_path,
      public_ip,
      schema_v
     ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`;

    const schemeValue = [
      getSellerId(),
      params.username,
      params.email,
      password,
      false,
      params.country,
      0,
      0,
      "Terms and conditions: Provide correct data for effective shipping.",
      params.terms_accepted,
      "https://example.com",
      "/avatar.jpg",
      "192.168.1.1",
      0,
    ];

    await conn.execute(schemeData, schemeValue);

    await conn.commit();

    res.status(200).send({ success: true });
  } catch (err) {
    await conn.rollback();

    _.error(err);

    throw new BadRequestError("failed");
  } finally {
    conn.release();
  }
};

export { createSellerMiddlewares, createSellerHandler };
