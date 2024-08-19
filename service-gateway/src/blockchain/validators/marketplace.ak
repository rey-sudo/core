use aiken/dict
use aiken/hash.{Blake2b_224, Hash}
use aiken/interval
use aiken/list
use aiken/string
use aiken/transaction.{OutputReference, ScriptContext, Spend, Transaction, InlineDatum } as tx
use aiken/transaction.{Input, NoDatum, Output, TransactionId}
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

type POSIXTime =
  Int

type StateMachineDatum {
  state: Int,
  seller: Hash<Blake2b_224, VerificationKey>,
  collateral: Int,
  price: Int,
  buyer: Option<Hash<Blake2b_224, VerificationKey>>,
  range: Option<POSIXTime>,
}

type StateMachineInput {
  Cancel
  Locking { buyer_param: Hash<Blake2b_224, VerificationKey>, range_param: POSIXTime }
  Return
  Shipping
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
          (StateMachineDatum { state, seller, collateral, price, .. }, Cancel) -> {
            let must_be_state = state == 0  

            let must_be_signed =
              list.has(context.transaction.extra_signatories, seller)

            expect Some(sm_input) = 
              list.find(context.transaction.inputs, fn(input) { input.output_reference == utxo_ref })            

            expect Some(sm_output) = 
              list.find(context.transaction.outputs, fn(output) { output.address == sm_input.output.address })
            
            let must_be_policy = list.has(value.policies(sm_output.value), threadtoken)

            let new_data: Data = StateMachineDatum {
              state : -1,
              seller : seller,
              collateral : collateral,
              price: price,
              buyer: None,
              range: None
            }

            let new_datum = InlineDatum(new_data)

            let must_be_datum = new_datum == sm_output.datum

            must_be_state? && must_be_signed? && must_be_policy? && must_be_datum?
          }

          (StateMachineDatum { state, seller, collateral, price, buyer, .. }, Locking { buyer_param, range_param }) -> {
            let must_be_state = state == 0

            let must_be_empty = buyer == None

            let must_be_signed =
              list.has(context.transaction.extra_signatories, buyer_param)

            expect Some(sm_input) = 
                list.find(context.transaction.inputs, fn(input) { input.output_reference == utxo_ref })

            
            expect Some(sm_output) = 
              list.find(context.transaction.outputs, fn(output) { output.address == sm_input.output.address })


            let must_be_policy = list.has(value.policies(sm_output.value), threadtoken)

            let spending_lovelace = value.lovelace_of(sm_output.value)

            trace string.from_int(spending_lovelace)

            let must_be_price = spending_lovelace >= price

            let new_data: Data = StateMachineDatum {
              state : 1,
              seller : seller,
              collateral : collateral,
              price: price,
              buyer: Some(buyer_param),
              range: Some(range_param)
            }

            let new_datum = InlineDatum(new_data)

            let must_be_datum = new_datum == sm_output.datum

            must_be_state? && must_be_empty? && must_be_signed? && must_be_policy? && must_be_price? && must_be_datum?
          }

          (StateMachineDatum { state, seller, collateral, price, buyer, range }, Return) -> {
            let must_be_state = state == 1

            let must_be_signed = when buyer is {
                                  Some(buyer) ->  list.has(context.transaction.extra_signatories, buyer)
                                  _ -> False
                                  }
              
            let must_be_time = when context.transaction.validity_range.lower_bound.bound_type is {
                                interval.Finite(tx_earliest_time) ->  when range is {
                                                                        Some(range_value) -> range_value <= tx_earliest_time
                                                                        _ -> False  
                                                                        }
                                _ -> False
                                }

            expect Some(sm_input) = 
              list.find(context.transaction.inputs, fn(input) { input.output_reference == utxo_ref })
                                         
            expect Some(sm_output) = 
              list.find(context.transaction.outputs, fn(output) { output.address == sm_input.output.address })
                             
            let must_be_policy = list.has(value.policies(sm_output.value), threadtoken)

            let spending_lovelace = value.lovelace_of(sm_output.value)

            let must_be_collateral = spending_lovelace == collateral

            let new_data: Data = StateMachineDatum {
              state : 0,
              seller : seller,
              collateral : collateral,
              price: price,
              buyer: None,
              range: None
            }
                  
            let new_datum = InlineDatum(new_data)
                  
            let must_be_datum = new_datum == sm_output.datum                                
            
            must_be_state? && must_be_signed? && must_be_time? && must_be_policy? && must_be_collateral? && must_be_datum?
          }

          (StateMachineDatum { state, .. }, Shipping) -> {
            state == 1
          }

          (StateMachineDatum { state, .. }, Received) -> {
            state == 2
          }
        }
      _ -> False
    }
  }
}


test cancel() {
  let own_ref =
    OutputReference {
      transaction_id: TransactionId {
        hash: #"ee220ba9258ef159bb44f0d3522b840a80475eac2df7176f2c513d59a2ead71f",
      },
      output_index: 0,
    }
  let threadtoken = #"99999999999999999999999999999999999999999999999999999999"
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
          value.from_lovelace(25_000_000),
          threadtoken,
          token_name,
          1,
        ),
        datum: NoDatum,
        reference_script: None,
      },
    }
  let new_data: Data = StateMachineDatum {
      state : -1,
      seller : #"00000000000000000000000000000000000000000000000000000000",
      collateral : 25000000,
      price: 50000000,
      buyer: None,
      range: None
  }  
  let new_datum = InlineDatum(new_data)
  let output =
    Output {
      address: Address {
        payment_credential: ScriptCredential(own_val_hash),
        stake_credential: None,
      },
      value: value.add(
        value.from_lovelace(25_000_000),
        threadtoken,
        token_name,
        1,
      ),
      datum: new_datum,
      reference_script: None,
    }
  let script_purpose = Spend(own_ref)
  let redeemer = Cancel
  let old_datum =
    StateMachineDatum {
      state: 0,
      seller: #"00000000000000000000000000000000000000000000000000000000",
      collateral: 25000000,
      price: 50000000,
      buyer: None,
      range: None
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
      extra_signatories: [old_datum.seller],
      redeemers: [],
      datums: dict.new(),
      id: TransactionId {
        hash: #"0000000000000000000000000000000000000000000000000000000000000000",
      },
    }
  let ctx = ScriptContext { purpose: script_purpose, transaction: tx }
  statemachine(threadtoken, old_datum, redeemer, ctx)
}


test locking() {
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
  
  let new_buyer = #"77777777777777777777777777777777777777777777777777777777"  
  let new_data: Data = StateMachineDatum {
      state : 1,
      seller : #"00000000000000000000000000000000000000000000000000000000",
      collateral : 25000000,
      price: 50000000,
      buyer: Some(new_buyer),
      range: Some(1672843961000)
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
  let redeemer = Locking {  buyer_param: new_buyer,  range_param: 1672843961000  }
  let old_datum =
    StateMachineDatum {
      state: 0,
      seller: #"00000000000000000000000000000000000000000000000000000000",
      collateral: 25000000,
      price: 50000000,
      buyer: None,
      range: None
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
  statemachine(threadtoken, old_datum, redeemer, ctx)
}


test return() {
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
  let range_param = 1672843961000
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
  
  let new_buyer = #"77777777777777777777777777777777777777777777777777777777"  
  let new_data: Data = StateMachineDatum {
      state : 0,
      seller : #"00000000000000000000000000000000000000000000000000000000",
      collateral : 25000000,
      price: 50000000,
      buyer: None,
      range: None
  }  
  let new_datum = InlineDatum(new_data)
  let output =
    Output {
      address: Address {
        payment_credential: ScriptCredential(own_val_hash),
        stake_credential: None,
      },
      value: value.add(
        value.from_lovelace(25_000_000),
        threadtoken,
        token_name,
        1,
      ),
      datum: new_datum,
      reference_script: None,
    }
  let redeemer = Return
  let old_datum =
    StateMachineDatum {
      state: 1,
      seller: #"00000000000000000000000000000000000000000000000000000000",
      collateral: 25000000,
      price: 50000000,
      buyer: Some(new_buyer),
      range: Some(range_param)
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
      validity_range: interval.after(range_param),
      extra_signatories: [new_buyer],
      redeemers: [],
      datums: dict.new(),
      id: TransactionId {
        hash: #"0000000000000000000000000000000000000000000000000000000000000000",
      },
    }
  let ctx = ScriptContext { purpose: script_purpose, transaction: tx }
  statemachine(threadtoken, old_datum, redeemer, ctx)
}