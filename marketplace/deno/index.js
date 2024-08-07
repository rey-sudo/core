import { ColdWallet, Core, Blaze, Blockfrost } from "@blaze-cardano/sdk";

let externalWallet = Core.addressFromBech32(
  "addr_test1qppygdhzm0t7nnlclmds3dy0wc3du870dpy48juu0xxuu2aefdfvc4e0785y7vfhwlmsn3rn26mzvv9md0mhnkpjlc4s0jshh4"
);

const targetWallet = Core.addressFromBech32(
  "addr_test1qp539v654clv34y7k6zwrtxzczwvzz0dudmgfy5rt3qvjf2hrg74pzy4umh8udkhshpqmwdzluk6zvr5tcrj8h74re2q2yavu8"
);

const provider = new Blockfrost({
  network: "cardano-preview",
  projectId: "previewgTjbjYtdKdOcNmhtu6H9snNl3DhnaxQf",
});



const wallet = new ColdWallet(externalWallet, 2, provider);

console.log("Your blaze address: ", wallet.address.toBech32());

const blaze = await Blaze.from(provider, wallet);

const tx = await blaze
  .newTransaction()
  .payLovelace(targetWallet, 10n * 1_000_000n)
  .addRequiredSigner("424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b")
  .complete();

// Dump the transaction for you to submit securely
console.log(tx.toCbor());



//"previewgTjbjYtdKdOcNmhtu6H9snNl3DhnaxQf"