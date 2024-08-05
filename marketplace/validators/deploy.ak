use aiken/dict
use aiken/list
use aiken/transaction.{OutputReference, ScriptContext, Transaction} as tx
use aiken/transaction/value

// Define the minting redeemer
type ThreadTokenInput {
  Mint
  Burn
}

validator(token_name: ByteArray, utxo_ref: OutputReference) {
  fn threadtoken(redeemer: ThreadTokenInput, ctx: ScriptContext) -> Bool {
    let ScriptContext { transaction, purpose } = ctx
    expect tx.Mint(policy_id) = purpose
    let Transaction { inputs, mint, .. } = transaction
    expect [(asset_name, amount)] =
      mint
        |> value.from_minted_value
        |> value.tokens(policy_id)
        |> dict.to_list()

    when redeemer is {
      Mint -> {
        expect Some(_input) =
          list.find(inputs, fn(input) { input.output_reference == utxo_ref })
        amount == 1 && asset_name == token_name
      }

      Burn -> amount == -1 && asset_name == token_name
    }
  }
}

type MachineStateDatum {
  seller: Hash<Blake2b_224, VerificationKey>,
}

type Redeemer {
  msg: ByteArray,
}

validator {
  fn machinestate(
    datum: MachineStateDatum,
    redeemer: Redeemer,
    context: ScriptContext,
  ) -> Bool {
    trace string.from_bytearray(redeemer.msg)
    let must_say_hello = redeemer.msg == "Hello, World!"
    let must_be_signed =
      list.has(context.transaction.extra_signatories, datum.seller)
    must_say_hello? && must_be_signed?
  }
}
