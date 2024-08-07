import { ColdWallet, Core, Blaze, Blockfrost } from "@blaze-cardano/sdk";



let address = Core.addressFromBech32(
  "addr_test1qppygdhzm0t7nnlclmds3dy0wc3du870dpy48juu0xxuu2aefdfvc4e0785y7vfhwlmsn3rn26mzvv9md0mhnkpjlc4s0jshh4"
);


const externalWallet = Core.addressFromBech32(
  "addr_test1qp539v654clv34y7k6zwrtxzczwvzz0dudmgfy5rt3qvjf2hrg74pzy4umh8udkhshpqmwdzluk6zvr5tcrj8h74re2q2yavu8"
);

const provider = new Blockfrost({
  network: "cardano-preview",
  projectId: "previewgTjbjYtdKdOcNmhtu6H9snNl3DhnaxQf",
});



const wallet = new ColdWallet(address, 2, provider);

console.log("Your blaze address: ", wallet.address.toBech32());

const blaze = await Blaze.from(provider, wallet);


const tx = await blaze
  .newTransaction()
  .payLovelace(externalWallet, 5n * 1_000_000n)
  .complete();

// Dump the transaction for you to submit securely
console.log(`Please sign and submit this transaction: ${tx.toCbor()}`);
