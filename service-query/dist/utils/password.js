import { promisify } from "util";
import { scrypt, randomBytes } from "crypto";
const scryptAsync = promisify(scrypt);
export class Password {
    static async toHash(password) {
        const salt = randomBytes(8).toString("hex");
        const buf = (await scryptAsync(password, salt, 64));
        return `${buf.toString("hex")}.${salt}`;
    }
    static async compare(storedPassword, suppliedPassword) {
        const [hashedPassword, salt] = storedPassword.split(".");
        const buf = (await scryptAsync(suppliedPassword, salt, 64));
        return buf.toString("hex") === hashedPassword;
    }
}
export const hashPassword = async (password) => Password.toHash(password);
export const comparePassword = async (a, b) => Password.compare(a, b);
