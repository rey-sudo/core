"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Encrypto = exports.Password = void 0;
const crypto_1 = require("crypto");
const util_1 = require("util");
const scryptAsync = (0, util_1.promisify)(crypto_1.scrypt);
const secretKey = process.env.ENCRYPTO_KEY;
const algorithm = "aes-256-ctr";
class Password {
    static async toHash(password) {
        const salt = (0, crypto_1.randomBytes)(8).toString("hex");
        const buf = (await scryptAsync(password, salt, 64));
        return `${buf.toString("hex")}.${salt}`;
    }
    static async compare(storedPassword, suppliedPassword) {
        const [hashedPassword, salt] = storedPassword.split(".");
        const buf = (await scryptAsync(suppliedPassword, salt, 64));
        return buf.toString("hex") === hashedPassword;
    }
}
exports.Password = Password;
class Encrypto {
    static encrypt = (input) => {
        const iv = (0, crypto_1.randomBytes)(16);
        const cipher = (0, crypto_1.createCipheriv)(algorithm, secretKey, iv);
        const encrypted = Buffer.concat([cipher.update(input), cipher.final()]);
        const output = Buffer.from(JSON.stringify({
            iv: iv.toString("hex"),
            content: encrypted.toString("hex"),
        }));
        return output.toString("base64");
    };
    static decrypt = (output) => {
        const hash = JSON.parse(Buffer.from(output, "base64").toString());
        const decipher = (0, crypto_1.createDecipheriv)(algorithm, secretKey, Buffer.from(hash.iv, "hex"));
        const decrypter = Buffer.concat([
            decipher.update(Buffer.from(hash.content, "hex")),
            decipher.final(),
        ]);
        return decrypter.toString();
    };
}
exports.Encrypto = Encrypto;
