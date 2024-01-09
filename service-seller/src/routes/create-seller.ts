import { BadRequestError } from "../../global";
import { Request, Response } from "express";
import DB from "../db";

const createSellerMiddlewares: any = [];

const createSellerHandler = async (req: Request, res: Response) => {
  const conn = await DB.client.getConnection();
  try {
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
      public_ip
     ) VALUES (?, ?, UNHEX(SHA2(?, 256)), UNHEX(SHA2(?, 256)), ?, ?, ?, ?, ?, ?, ?)`;

    const schemeValue = [
      "unique_seller_id",
      "JohnDoe",
      "john.doe@example.com",
      "password123",
      false,
      "United States",
      0,
      0,
      "Terms and conditions text here.",
      "https://example.com/avatar.jpg",
      "192.168.1.1",
    ];

    const result = await conn.execute(schemeData, schemeValue);

    console.log(result);

    await conn.commit();

    res.status(200).send({});
  } catch (err) {
    await conn.rollback();
    console.log(err);
  } finally {
    conn.release();
  }

  res.status(200).send({});
};

export { createSellerMiddlewares, createSellerHandler };
