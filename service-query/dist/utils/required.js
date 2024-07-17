import { NotAuthorizedError } from "../errors/NotAuthorizedError";
export const requireAuth = (req, res, next) => {
    if (!req.sellerData) {
        throw new NotAuthorizedError();
    }
    next();
};
