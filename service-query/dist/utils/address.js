export const getPublicAddress = (req, res, next) => {
    req.publicAddress = req.headers["cf-connecting-ip"] || req.ip;
    next();
};
