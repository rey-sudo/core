import { _ } from "./pino";
import jwt from "jsonwebtoken";
const sellerMiddleware = (req, res, next) => {
    if (!req.session?.jwt) {
        return next();
    }
    try {
        const sessionData = jwt.verify(req.session.jwt, process.env.SELLER_JWT_KEY);
        if (sessionData.role !== "SELLER") {
            return next();
        }
        req.sellerData = sessionData;
    }
    catch (err) {
        _.error(err);
    }
    next();
};
export { sellerMiddleware };
