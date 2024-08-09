use aiken/dict
use aiken/hash.{Blake2b_224, Hash}
use aiken/list
use aiken/string
use aiken/transaction.{OutputReference, ScriptContext, Spend, Transaction} as tx
use aiken/transaction/credential.{VerificationKey}
use aiken/transaction/value

type ThreadTokenInput {
  Mint
  Burn
}

validator(token_name: ByteArray, utxo_ref: OutputReference) {
  fn threadtoken(redeemer: ThreadTokenInput, ctx: ScriptContext) -> Bool {
    let ScriptContext { transaction, purpose } = ctx
    expect tx.Mint(policy_id) = purpose
    let Transaction { inputs, mint, .. } = transaction
    expect [Pair(asset_name, amount)] =
      mint
        |> value.from_minted_value
        |> value.tokens(policy_id)
        |> dict.to_pairs()

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

type StateMachineDatum {
  state: Int,
  seller: Hash<Blake2b_224, VerificationKey>,
}

type StateMachineInput {
  Waiting
  Locking { buyer: ByteArray }
  Delivered
  Received
}

validator(threadtoken: ByteArray) {
  fn statemachine(
    datum: StateMachineDatum,
    redeemer: StateMachineInput,
    context: ScriptContext,
  ) -> Bool {
    when context.purpose is {
      Spend(_) ->
        when (datum, redeemer) is {
          (StateMachineDatum { state, seller }, Waiting) -> {
            let must_be_state = state == 0
            let must_be_signed =
              list.has(context.transaction.extra_signatories, seller)

            let lovelace_spending =
              list.foldl(
                context.transaction.inputs,
                0,
                fn(n, total) { value.lovelace_of(n.output.value) + total },
              )

            trace string.from_int(lovelace_spending)

            must_be_state? && must_be_signed?
          }

          (StateMachineDatum { state, .. }, Locking { buyer }) -> True

          (StateMachineDatum { state, .. }, Delivered) -> True

          (StateMachineDatum { state, .. }, Received) -> True
        }
      _ -> False
    }
  }
}
