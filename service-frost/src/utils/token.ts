import jwt from "jsonwebtoken";

const signToken = (params: object) => {
  return jwt.sign(params, process.env.AUDITS_JWT_KEY!, {
    expiresIn: process.env.TOKEN_EXPIRATION_TIME!,
  });
};

const signTokenByTime = (params: object, expiresIn: string) => {
  return jwt.sign(params, process.env.AUDITS_JWT_KEY!, {
    expiresIn,
  });
};


const decodeTokenData = (token: string) => {
  let data = null;
  jwt.verify(token, process.env.AUDITS_JWT_KEY!, (err, decoded) => {
    if (err) {
      return null;
    } else {
      data = decoded;
    }
  });

  return data;
};



export { signToken, signTokenByTime, decodeTokenData }