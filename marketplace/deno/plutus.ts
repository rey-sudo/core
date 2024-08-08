// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-nocheck
import { type Script } from "@blaze-cardano/core";
import { applyParamsToScript, cborToScript } from "@blaze-cardano/uplc";

export interface MarketplaceMachinestate {
  new (): Script;
  datum: { state: bigint; seller: string };
  redeemer:
    | "Waiting"
    | { Locking: { buyer: string } }
    | "Delivered"
    | "Received";
}

export const MarketplaceMachinestate = Object.assign(
  function () {
    return cborToScript(
      "590193010000323232323232322323223225333007323232323232533300d300b300e375400a264a66601c601800620022940c8cc004004dd61809980a180a180a180a180a180a180a180a18081baa30133010375401044a66602400229404c94ccc040cdc79bae301500200414a2266006006002602a002264a66601c66e1d2002300f375400c264944dd7180998081baa006132533300f3370e900218081baa00714a2264944dd6980a18089baa006375a602660206ea8014dd6980918079baa004375c602260240046eb4c040004c030dd50009807001180698070009980580319805801a5eb8052613656325333006300400115333009300837540042930b0a99980319b87480080044c8c94ccc02cc03800852616375c601800260106ea800854ccc018cdc3a40080022a66601260106ea800852616153330063370e90030008a99980498041baa00214985858c018dd5000a999801980098021baa002132323232533300a300d002149858dd7180580098058011bad3009001300537540042c6e1d20005734aae7555cf2ab9f5740ae855d101",
      "PlutusV2",
    );
  },
  {
    datum: {
      "title": "MachineStateDatum",
      "anyOf": [{
        "title": "MachineStateDatum",
        "dataType": "constructor",
        "index": 0,
        "fields": [{ "dataType": "integer", "title": "state" }, {
          "dataType": "bytes",
          "title": "seller",
        }],
      }],
    },
  },
  {
    redeemer: {
      "title": "MachineStateInput",
      "anyOf": [{
        "title": "Waiting",
        "dataType": "constructor",
        "index": 0,
        "fields": [],
      }, {
        "title": "Locking",
        "dataType": "constructor",
        "index": 1,
        "fields": [{ "dataType": "bytes", "title": "buyer" }],
      }, {
        "title": "Delivered",
        "dataType": "constructor",
        "index": 2,
        "fields": [],
      }, {
        "title": "Received",
        "dataType": "constructor",
        "index": 3,
        "fields": [],
      }],
    },
  },
) as unknown as MarketplaceMachinestate;

export interface MarketplaceThreadtoken {
  new (
    tokenName: string,
    utxoRef: { transactionId: { hash: string }; outputIndex: bigint },
  ): Script;
  redeemer: "Mint" | "Burn";
}

export const MarketplaceThreadtoken = Object.assign(
  function (
    tokenName: string,
    utxoRef: { transactionId: { hash: string }; outputIndex: bigint },
  ) {
    return cborToScript(
      applyParamsToScript(
        "5901f5010000323232323232322322232323225333009323232533300c3007300d3754002264646464a666026602c00426464a666024601a60266ea803854ccc048c034c04cdd5191980080080311299980b8008a60103d87a80001323253330163375e603660306ea800804c4cdd2a40006603400497ae0133004004001301b002301900115333012300c00113371e00402029405854ccc048cdc3800a4002266e3c0080405281bad3013002375c60220022c602800264a66601e601260206ea800452f5bded8c026eacc050c044dd500099191980080099198008009bab3016301730173017301700522533301500114bd6f7b630099191919299980b19b91488100002153330163371e9101000021003100513301a337606ea4008dd3000998030030019bab3017003375c602a0046032004602e00244a666028002298103d87a800013232323253330153372200e0042a66602a66e3c01c0084cdd2a4000660326e980052f5c02980103d87a80001330060060033756602c0066eb8c050008c060008c058004dd7180998081baa00337586024002601c6ea800858c040c044008c03c004c02cdd50008a4c26cac64a66601060060022a66601660146ea8010526161533300830020011533300b300a37540082930b0b18041baa003370e90011b8748000dd7000ab9a5573aaae7955cfaba05742ae89",
        [tokenName, utxoRef],
        {
          "dataType": "list",
          "items": [{ "dataType": "bytes" }, {
            "title": "OutputReference",
            "description":
              "An `OutputReference` is a unique reference to an output on-chain. The `output_index`\n corresponds to the position in the output list of the transaction (identified by its id)\n that produced that output",
            "anyOf": [{
              "title": "OutputReference",
              "dataType": "constructor",
              "index": 0,
              "fields": [{
                "title": "transactionId",
                "description":
                  "A unique transaction identifier, as the hash of a transaction body. Note that the transaction id\n isn't a direct hash of the `Transaction` as visible on-chain. Rather, they correspond to hash\n digests of transaction body as they are serialized on the network.",
                "anyOf": [{
                  "title": "TransactionId",
                  "dataType": "constructor",
                  "index": 0,
                  "fields": [{ "dataType": "bytes", "title": "hash" }],
                }],
              }, { "dataType": "integer", "title": "outputIndex" }],
            }],
          }],
        } as any,
      ),
      "PlutusV2",
    );
  },
  {
    redeemer: {
      "title": "ThreadTokenInput",
      "anyOf": [{
        "title": "Mint",
        "dataType": "constructor",
        "index": 0,
        "fields": [],
      }, {
        "title": "Burn",
        "dataType": "constructor",
        "index": 1,
        "fields": [],
      }],
    },
  },
) as unknown as MarketplaceThreadtoken;
