import { BadRequestError } from "../errors";
import { Request, Response } from "express";
import { createToken } from "../utils/token";
import { UserToken, userMiddleware } from "../utils/user";
import { _ } from "../utils/pino";
import DB from "../db";
import { getUserId } from "../utils/nano";
const Cardano = require("@emurgo/cardano-serialization-lib-nodejs");

const loginUserMiddlewares: any = [userMiddleware];

const loginUserHandler = async (req: Request, res: Response) => {
  let connection = null;
  let params = req.body;

  console.log(req.body);

  try {
    const verifySignature = (
      address: string,
      message: string,
      signature: any
    ) => {
      const pubKey = Cardano.PublicKey.from_bech32(address);
      const sig = Cardano.Ed25519Signature.from_bech32(signature);
      const messageBytes = Buffer.from(message, "utf8");
      return pubKey.verify(messageBytes, sig);
    };

    const address = params.address;
    const message = params.message;
    const signature = params.signature;
    const pubkeyhash = "server";

    if (!verifySignature(address, message, signature)) {
      throw new BadRequestError("AUTH_FAILED");
    }

    ///////////////////////////////////////////////////////

    connection = await DB.client.getConnection();

    await connection.beginTransaction();

    const userId = getUserId();

    const schemeData = `
    INSERT INTO users (
      id,
      username,
      address,
      pubkeyhash,
      country,
      terms_accepted,
      public_ip,
      schema_v
     ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)`;

    const schemeValue = [
      userId,
      params.username,
      address,
      pubkeyhash,
      params.country,
      params.terms_accepted,
      "192.168.1.1",
      0,
    ];

    const userData: UserToken = {
      id: userId,
      role: "user",
      address,
      pubkeyhash,
      country: "ip",
      username: params.username,
    };

    req.session = {
      jwt: createToken(userData),
    };

    await connection.execute(schemeData, schemeValue);

    await connection.commit();

    res.status(200).send({ success: true, data: userData });
  } catch (err) {
    await connection.rollback();
    _.error(err);
  } finally {
    connection.release();
  }
};

export { loginUserMiddlewares, loginUserHandler };
