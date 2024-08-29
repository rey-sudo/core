import { Lucid, Blockfrost } from "@lucid-evolution/lucid";

const lucid = await Lucid(
  new Blockfrost(
    "https://cardano-preprod.blockfrost.io/api/v0",
    "preprodkWGZGJyPVdkWvaCkGzqJRY2R2BM4Gopt"
  ),
  "Preprod"
);

const externalWallet =
  "addr_test1qrg0fvp99s79f58vy8lxqrz3fzwmn4w9xnc54lpjy74847v04wk5sd4fhk5jur50npqse22mjn4we4r4l7uxfpdggrcsf7cf5y";

const targetWallet =
  "addr_test1qpqzsucnvps0v44cpqk8j74gqhkgwznckkw4yq4rt5szf0wzyv3wg0ry38rw8a86dey7j2mxfp72j298ygcxzawstrlsk6d79p";

const utxos = await lucid.utxosAt(externalWallet);

lucid.selectWallet.fromAddress(externalWallet, utxos);

const tx = await lucid
  .newTx()
  .pay.ToAddress(targetWallet, { lovelace: 5000000n })
  .complete();

const transaction = await tx.toCBOR();

console.log(transaction);
