import { BadRequestError } from "../../global";
import { Request, Response } from "express";
import DB from "../db";
import { getSellerId } from "../utils/nano";

const createSellerMiddlewares: any = [];

const createSellerHandler = async (req: Request, res: Response) => {
  let conn = null;

  const params = req.body;

  try {
    conn = await DB.client.getConnection();

    await conn.beginTransaction();

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
      avatar_url,
      public_ip,
      schema_v
     ) VALUES (?, ?, UNHEX(SHA2(?, 256)), UNHEX(SHA2(?, 256)), ?, ?, ?, ?, ?, ?, ?, ?)`;

    const schemeValue = [
      getSellerId(),
      params.nickname,
      params.email,
      params.password,
      false,
      params.country,
      0,
      0,
      "Terms and conditions: Provide correct data for effective shipping.",
      "https://example.com/avatar.jpg",
      "192.168.1.1",
      0
    ];

    const result = await conn.execute(schemeData, schemeValue);

    console.log(result);

    await conn.commit();

    res.status(200).send({});
  } catch (err) {
    await conn.rollback();

    console.log(err);

    res.status(500).send({ message: "unprocessed operation" });
  } finally {
    conn.release();
  }
};

export { createSellerMiddlewares, createSellerHandler };
