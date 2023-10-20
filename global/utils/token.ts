import jwt from "jsonwebtoken";
import { TokenPayload } from "../types/TokenPayload";
import { Encrypto } from "./crypto";

export const signToken = (params: object) => {
  return jwt.sign(params, process.env.JWT_KEY!, {
    expiresIn: process.env.TOKEN_EXPIRATION_TIME!,
  });
};

export const signTokenByTime = (params: object, expiresIn: string) => {
  return jwt.sign(params, process.env.JWT_KEY!, {
    expiresIn,
  });
};

export const decodeTempToken = (token: string) => {
  const decode = Encrypto.decrypt(token);

  const output = decodeTokenData(decode)

  if(!output){
   return null 
  }

  return output as TokenPayload
  
};

export const decodeTokenData = (token: string) => {
  let data = null;
  jwt.verify(token, process.env.JWT_KEY!, (err, decoded) => {
    if (err) {
      return null;
    } else {
      data = decoded;
    }
  });

  return data;
};
