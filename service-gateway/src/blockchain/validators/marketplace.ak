use aiken/dict
use aiken/fuzz
use aiken/hash.{Blake2b_224, Hash}
use aiken/interval
use aiken/list
use aiken/pairs
use aiken/string
use aiken/transaction.{OutputReference, ScriptContext, Spend, Transaction, InlineDatum } as tx
use aiken/transaction.{Input, NoDatum, Output, Datum, TransactionId}
use aiken/transaction/credential.{Address, ScriptCredential, VerificationKey}
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
  collateral: Int,
  price: Int,
  buyer: Option<Hash<Blake2b_224, VerificationKey>>,
}

type StateMachineInput {
  Locking { buyer_param: Hash<Blake2b_224, VerificationKey> }
  Delivered
  Received
}

validator(threadtoken: value.PolicyId) {
  fn statemachine(
    datum: StateMachineDatum,
    redeemer: StateMachineInput,
    context: ScriptContext,
  ) -> Bool {
    when context.purpose is {
      Spend(utxo_ref) ->
        when (datum, redeemer) is {
          (StateMachineDatum { state, seller, collateral, price, buyer }, Locking { buyer_param }) -> {
            let must_be_state = state == 0
            let must_be_empty = buyer == None
            let must_be_signed =
              list.has(context.transaction.extra_signatories, buyer_param)

            expect Some(sm_input) = 
                list.find(context.transaction.inputs, fn(input) { input.output_reference == utxo_ref })

            
            expect Some(sm_output) = 
              list.find(context.transaction.outputs, fn(output) { output.address == sm_input.output.address })


            let must_be_policy = list.has(value.policies(sm_output.value), threadtoken)

            let speding_lovelace = value.lovelace_of(sm_output.value)

            trace string.from_int(speding_lovelace)

            let must_be_price = speding_lovelace >= price

            let new_data: Data = StateMachineDatum {
              state : 1,
              seller : seller,
              collateral : collateral,
              price: price,
              buyer: Some(buyer_param)
            }

            let new_datum = InlineDatum(new_data)

            let must_be_datum = new_datum == sm_output.datum

            must_be_state? && must_be_signed? && must_be_price? && must_be_policy? && must_be_datum?
          }

          (StateMachineDatum { state, .. }, Delivered) -> True

          (StateMachineDatum { state, .. }, Received) -> True
        }
      _ -> False
    }
  }
}

test deploy() {
  let own_ref =
    OutputReference {
      transaction_id: TransactionId {
        hash: #"ee220ba9258ef159bb44f0d3522b840a80475eac2df7176f2c513d59a2ead71f",
      },
      output_index: 0,
    }
  let script_purpose = Spend(own_ref)
  let threadtoken = #"99999999999999999999999999999999999999999999999999999999"
  let token_name = #"02"
  let own_val_hash = #"88888888888888888888888888888888888888888888888888888888"
  let new_buyer = #"77777777777777777777777777777777777777777777777777777777"
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
          threadtoken,
          token_name,
          1,
        ),
        datum: NoDatum,
        reference_script: None,
      },
    }
  let new_data: Data = StateMachineDatum {
      state : 1,
      seller : #"00000000000000000000000000000000000000000000000000000000",
      collateral : 25000000,
      price: 50000000,
      buyer: Some(new_buyer)
  }  
  let new_datum = InlineDatum(new_data)
  let output =
    Output {
      address: Address {
        payment_credential: ScriptCredential(own_val_hash),
        stake_credential: None,
      },
      value: value.add(
        value.from_lovelace(50_000_000),
        threadtoken,
        token_name,
        1,
      ),
      datum: new_datum,
      reference_script: None,
    }
  let redeemer = Locking {  buyer_param: new_buyer  }
  let fake_buyer = #"55555555555555555555555555555555555555555555555555555555"
  let current_datum =
    StateMachineDatum {
      state: 0,
      seller: #"00000000000000000000000000000000000000000000000000000000",
      collateral: 25000000,
      price: 50000000,
      buyer: None
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
      extra_signatories: [new_buyer],
      redeemers: [],
      datums: dict.new(),
      id: TransactionId {
        hash: #"0000000000000000000000000000000000000000000000000000000000000000",
      },
    }
  let ctx = ScriptContext { purpose: script_purpose, transaction: tx }
  statemachine(threadtoken, current_datum, redeemer, ctx)
}
