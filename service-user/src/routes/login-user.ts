import { BadRequestError } from "../errors";
import { Request, Response } from "express";
import { createToken } from "../utils/token";
import { UserToken, userMiddleware } from "../utils/user";
import { _ } from "../utils/pino";
import { getUserId } from "../utils/nano";
import Cardano from "@emurgo/cardano-serialization-lib-nodejs";
import DB from "../db";
const cbor = require("cbor");

const loginUserMiddlewares: any = [userMiddleware];

const loginUserHandler = async (req: Request, res: Response) => {
  let connection = null;
  let params = req.body;

  console.log(params);

  try {
    const address = "na";
    const signature = params.signature;
    const pubkeyhash = "server";

    try {
      const verifySignature = (signature: any) => {
        const cborPubKey = cbor.decodeFirstSync(
          Buffer.from(signature.key, "hex")
        );

        const decodedPubKey = cborPubKey.get(-2);

        const pubKey = Cardano.PublicKey.from_bytes(decodedPubKey);

        ////////////////////////////////////////////////////

        const cborSignature = cbor.decodeFirstSync(
          Buffer.from(signature.signature, "hex")
        );

        const decodedSignature = cborSignature[3];

        const sig = Cardano.Ed25519Signature.from_bytes(decodedSignature);

        ////////////////////////////////////////////////////

        const message = Buffer.from(
          "PLEASE SIGN TO AUTHENTICATE IN PAIRFY",
          "utf8"
        );

        return pubKey.verify(message, sig);
      };

      const result = verifySignature(signature);

      console.log(result);
    } catch (err) {
      console.log(err);

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
    console.log(err);
    await connection.rollback();
    _.error(err);
  } finally {
    connection.release();
  }
};

export { loginUserMiddlewares, loginUserHandler };
