use aiken/dict
use aiken/hash.{Blake2b_224, Hash}
use aiken/list
use aiken/string
use aiken/transaction.{OutputReference, ScriptContext, Spend, Transaction} as tx
use aiken/transaction/credential.{VerificationKey, Address, ScriptCredential}
use aiken/transaction/value

//////////////////TEST////////////////////////////
/// 

use aiken/fuzz
use aiken/interval
use aiken/pairs
use aiken/transaction.{
  Input, NoDatum, Output, 
  TransactionId,
}

/// 
/// 
///////////////////////////////////////////////
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





test spend_validator() {
  let own_ref =
    OutputReference {
      transaction_id: TransactionId {
        hash: #"ee220ba9258ef159bb44f0d3522b840a80475eac2df7176f2c513d59a2ead71f",
      },
      output_index: 0,
    }
  let script_purpose = Spend(own_ref)
  let state_token_symbol =
    #"99999999999999999999999999999999999999999999999999999999"
  let token_name = #"02"
  let own_val_hash = #"88888888888888888888888888888888888888888888888888888888"
  let input_utxo =
    Input {
      output_reference: own_ref,
      output: Output {
        address: Address {
          payment_credential: ScriptCredential(own_val_hash),
          stake_credential: None,
        },
        value: value.add(
          value.from_lovelace(2_000_000),
          state_token_symbol,
          token_name,
          1,
        ),
        datum: NoDatum,
        reference_script: None,
      },
    }
  let output =
    Output {
      address: Address {
        payment_credential: ScriptCredential(own_val_hash),
        stake_credential: None,
      },
      value: value.add(
        value.from_lovelace(2_000_000),
        state_token_symbol,
        token_name,
        1,
      ),
      datum: NoDatum,
      reference_script: None,
    }

  let redeemer = Waiting

  let datum = StateMachineDatum {
    state: 0,
    seller: #"00000000000000000000000000000000000000000000000000000000"
  }
  let tx = 
    Transaction {
      inputs: [input_utxo],
      reference_inputs: [],
      outputs: [output],
      fee: value.zero(),
      mint: value.zero() |> value.to_minted_value(),
      certificates: [],
      withdrawals: [],
      validity_range: interval.everything(),
      extra_signatories: [datum.seller],
      redeemers: [],
      datums: dict.new(),
      id: TransactionId {
        hash: #"0000000000000000000000000000000000000000000000000000000000000000",
      },
    }
  let ctx = ScriptContext { purpose: script_purpose, transaction: tx }
  statemachine(state_token_symbol, datum, redeemer, ctx)
}
