import { hashPassword } from "../utils/password";
import { BadRequestError } from "../errors";
import { Request, Response } from "express";
import { getSellerId } from "../utils/nano";
import { createToken } from "../utils/token";
import { _ } from "../utils/pino";
import DB from "../db";

const createSellerMiddlewares: any = [];

const createSellerHandler = async (req: Request, res: Response) => {
  const params = req.body;

  try {
    const result  = await DB.client.begin(async (sql: any) => {
      const token = createToken({
        role: "create-seller",
        entity: "seller",
        email: params.email,
        username: params.nickname,
      });

      const password = await hashPassword(params.password);

      const [user] = await sql`
      insert into seller (
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
       ) values (
       ${getSellerId()}, 
       ${params.nickname},
       ${params.email},
       ${password},
       ${false},
       ${params.country},
       ${0},
       ${0},
       'Terms and conditions: Provide correct data for effective shipping.',
       'https://example.com',    
       '/avatar.jpg',
       '192.168.1.1',
       ${0})

       returning *
       `;

      return user;
    });

    res.status(200).send({ success: true, data: result  });
  } catch (err) {
    _.error(err);

    throw new BadRequestError("failed");
  }
};

export { createSellerMiddlewares, createSellerHandler };
