import jwt from "jsonwebtoken";


export const createToken = (params: object) => {
  return jwt.sign(params, process.env.SELLER_JWT_KEY!, {
    expiresIn: process.env.TOKEN_EXP_TIME!,
  });
};