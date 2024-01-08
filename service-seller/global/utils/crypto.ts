import { scrypt, randomBytes, createCipheriv, createDecipheriv } from "crypto";
import { promisify } from "util";

const scryptAsync = promisify(scrypt);
const secretKey: string = process.env.ENCRYPTO_KEY!;
const algorithm: string = "aes-256-ctr";

export class Password {
  static async toHash(password: string) {
    const salt = randomBytes(8).toString("hex");

    const buf = (await scryptAsync(password, salt, 64)) as Buffer;

    return `${buf.toString("hex")}.${salt}`;
  }

  static async compare(storedPassword: string, suppliedPassword: string) {
    const [hashedPassword, salt] = storedPassword.split(".");

    const buf = (await scryptAsync(suppliedPassword, salt, 64)) as Buffer;

    return buf.toString("hex") === hashedPassword;
  }
}

export class Encrypto {
  static encrypt = (input: string) => {
    const iv = randomBytes(16);

    const cipher = createCipheriv(algorithm, secretKey, iv);

    const encrypted = Buffer.concat([cipher.update(input), cipher.final()]);

    const output = Buffer.from(
      JSON.stringify({
        iv: iv.toString("hex"),
        content: encrypted.toString("hex"),
      })
    );

    return output.toString("base64");
  };

  static decrypt = (output: any) => {
    const hash = JSON.parse(Buffer.from(output, "base64").toString());

    const decipher = createDecipheriv(
      algorithm,
      secretKey,
      Buffer.from(hash.iv, "hex")
    );
    
    const decrypter = Buffer.concat([
      decipher.update(Buffer.from(hash.content, "hex")),
      decipher.final(),
    ]);

    return decrypter.toString();
  };
}
