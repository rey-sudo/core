import { customAlphabet } from "nanoid";

const getSlotId = customAlphabet("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ", 20);

export { getSlotId };
