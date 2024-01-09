import { customAlphabet } from "nanoid";

const getSellerId = customAlphabet("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ", 20);

const largePid = customAlphabet("0123456789ABCDEFGHJKLMNOPQRTUVWXYZ", 20);

export { getSellerId, largePid };
