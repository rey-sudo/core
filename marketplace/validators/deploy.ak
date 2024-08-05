///////////////////////////////////////
///////////////////////////////////////

use aiken/dict
use aiken/hash.{Blake2b_224, Hash}
use aiken/list
use aiken/transaction.{Output,
  OutputReference, ScriptContext, Transaction} as tx
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

type MachineStateDatum {
  state: Int,
  seller: Hash<Blake2b_224, VerificationKey>,
}

type MachineStateInput {
  Waiting
  Locking { buyer: ByteArray }
  Delivered
  Received
}

validator {
  fn machinestate(
    datum: MachineStateDatum,
    redeemer: MachineStateInput,
    context: ScriptContext,
  ) -> Bool {
    when (datum, redeemer) is {
      (MachineStateDatum { state, seller }, Waiting) -> {
        let newState = 0

        let must_be_state =
          when context.transaction.outputs is {
            [Output{datum: Some(MachineStateDatum { state: newState ..})..}, ..] ->
              true
            _ -> false
          }

        let must_be_signed =
          list.has(context.transaction.extra_signatories, seller)

        must_be_state? && must_be_signed?
      }

      _ -> false
    }
  }
}
