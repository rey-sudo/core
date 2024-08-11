import { Blockfrost, Lucid } from "lucid-cardano";
import {
    ColdWallet,
    Core,
    Data,
    Blaze,
    makeValue,
  } from "@blaze-cardano/sdk";
  import { applyParamsToScript, cborToScript } from "@blaze-cardano/uplc";

console.log(Blockfrost, Lucid);
console.log(Data);

const message: string = "Hello, TypeScript with Node.js!";
console.log(message);
