import { customAlphabet } from "nanoid";

const getProductId = customAlphabet("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ", 20);

export { getProductId };
