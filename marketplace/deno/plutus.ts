// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-nocheck
import { type Script } from "@blaze-cardano/core";
import { applyParamsToScript, cborToScript } from "@blaze-cardano/uplc";

export interface MarketplaceStatemachine {
  new (threadtoken: string): Script;
  datum: { state: bigint; seller: string };
  redeemer:
    | "Waiting"
    | { Locking: { buyer: string } }
    | "Delivered"
    | "Received";
}

export const MarketplaceStatemachine = Object.assign(
  function (threadtoken: string) {
    return cborToScript(
      applyParamsToScript(
        "5902da0100003232323232323223223232232322533300a32533300b3004300c37546002601a6ea80084c8c8c8c8c8c94ccc044c034c048dd50028991919299980a180800288010a503233300100137586006602c6ea8c00cc058dd5005a4000444a66603200420022666006006603800466e00c94ccc05cc040c060dd50008a400026eb4c070c064dd500099299980b9808180c1baa00114c0103d87a8000132330010013756603a60346ea8008894ccc070004530103d87a8000132323232533301d33722911000021533301d3371e91010000213374a9000198109ba80014bd700a6103d87a8000133006006003375a603c0066eb8c070008c080008c078004c8cc004004dd59806980c9baa300d30193754603800644a666036002298103d87a8000132323232533301c33722911000021533301c3371e91010000213374a9000198101ba60014bd700a6103d87a80001330060060033756603a0066eb8c06c008c07c008c074004004c8cc004004dd6180c180c980c980c980c980c980c980c980c980a9baa30023015375401444a66602e00229404c94ccc054cdc79bae301a00200514a226600600600260340024602e002264a666024601660266ea80184c9289bae30173014375400c264a66602666e1d20043014375400e29444c9289bad30183015375400c6eb4c05cc050dd50029bad3016301337540086eb8c054c058008dd6980a00098081baa0013012002301130120013300f0083300f0054bd700a5023010301100114984d958c94ccc024c01400454ccc030c02cdd50018a4c2c2a666012600400226464a66601c60220042930b1bae300f001300b37540062a66601266e1d20040011533300c300b37540062930b0a99980499b874801800454ccc030c02cdd50018a4c2c2c60126ea8008dc3a4004a66600a6002600c6ea80084c8c8c8c94ccc030c03c00852616375c601a002601a0046eb4c02c004c01cdd50010b1b8748000dd7000ab9a5573aaae7955cfaba05742ae881",
        [threadtoken],
        { "dataType": "list", "items": [{ "dataType": "bytes" }] } as any,
      ),
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
) as unknown as MarketplaceStatemachine;

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
