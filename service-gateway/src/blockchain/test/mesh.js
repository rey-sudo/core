import cbor from "cbor";
import {
  resolvePaymentKeyHash,
  resolvePlutusScriptAddress,
  BlockfrostProvider,
  MeshWallet,
  Transaction,
} from "@meshsdk/core";
import fs from "node:fs";

const blueprint = JSON.parse(fs.readFileSync("./plutus.json"));

const provider = new BlockfrostProvider(
  "preprodkWGZGJyPVdkWvaCkGzqJRY2R2BM4Gopt"
);

const externalWallet =
  "addr_test1qpqzsucnvps0v44cpqk8j74gqhkgwznckkw4yq4rt5szf0wzyv3wg0ry38rw8a86dey7j2mxfp72j298ygcxzawstrlsk6d79p";

const script = {
  code: cbor
    .encode(Buffer.from(blueprint.validators[0].compiledCode, "hex"))
    .toString("hex"),
  version: "V3",
};

const wallet = new MeshWallet({
  networkId: 0,
  fetcher: provider,
  key: {
    type: "address",
    address: externalWallet,
  },
});



const unsignedTx = await new Transaction({ initiator: wallet })
  .mint("1", policyId, tokenNameHex)
  .mintingScript(forgingScript)
  .build();
