use aiken/crypto.{VerificationKeyHash}
use aiken/collection/list
use cardano/transaction.{OutputReference, Transaction, NoDatum, InlineDatum, Input, Output}
use cardano/assets.{PolicyId}
use cardano/assets
use aiken/collection/dict
use cardano/address.{Address, Script}
use aiken/collection/dict

pub type ThreadTokenInput {
  Mint
  Burn
}

validator threadtoken(token_name: ByteArray, utxo_ref: OutputReference) {
   mint(redeemer: ThreadTokenInput, policy_id: PolicyId, tx: Transaction) -> Bool {
    let Transaction { inputs, mint, .. } = tx

    expect [Pair(asset_name, amount)] =
      mint
        |> assets.tokens(policy_id)
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

pub type StateMachineDatum {
  state: Int,
  seller: VerificationKeyHash,
  collateral: Int,
  price: Int,
  buyer: Option<VerificationKeyHash>,
  range: Option<POSIXTime>,
}

pub type StateMachineInput {
  Cancel
  Locking { buyer_param: VerificationKeyHash, range_param: POSIXTime }
  Return
  Shipping
  Received
}

validator statemachine(threadtoken: PolicyId) {
  spend(
    datum_opt: Option<StateMachineDatum>,
    redeemer: StateMachineInput,
    own_ref: OutputReference,
    transaction: Transaction,
  ) -> Bool {
        expect Some(datum) = datum_opt
        
        when (datum, redeemer) is {
          (StateMachineDatum { state, seller, collateral, price, .. }, Cancel) -> {
            let must_be_state = state == 0  

            let must_be_signed =
              list.has(transaction.extra_signatories, seller)

            expect Some(sm_input) = 
              list.find(transaction.inputs, fn(input) { input.output_reference == own_ref })            

            expect Some(sm_output) = 
              list.find(transaction.outputs, fn(output) { output.address == sm_input.output.address })
            
            let must_be_policy = list.has(assets.policies(sm_output.value), threadtoken)

            let new_data: Data = StateMachineDatum {
              state : -1,
              seller : seller,
              collateral : collateral,
              price: price,
              buyer: None,
              range: None
            }

            let must_be_datum = InlineDatum(new_data) == sm_output.datum

            and {
              must_be_state?,
              must_be_signed?,
              must_be_policy?,
              must_be_datum?
            }
          }

          _ -> False
        }
  }
}


test cancel() {
  let own_ref =
    OutputReference {
      transaction_id: #"ee220ba9258ef159bb44f0d3522b840a80475eac2df7176f2c513d59a2ead71f",
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
          payment_credential: Script(own_val_hash),
          stake_credential: None,
        },
        value: assets.add(
          assets.from_lovelace(25_000_000),
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
        payment_credential: Script(own_val_hash),
        stake_credential: None,
      },
      value: assets.add(
        assets.from_lovelace(25_000_000),
        threadtoken,
        token_name,
        1,
      ),
      datum: new_datum,
      reference_script: None,
    }
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

  let transaction =
    Transaction {
      inputs: [input_utxo],
      reference_inputs: [],
      outputs: [output],
      fee: 0,
      mint: transaction.placeholder.mint,
      certificates: [],
      withdrawals: [],
      validity_range: transaction.placeholder.validity_range,
      extra_signatories: [old_datum.seller],
      redeemers: [],
      datums: transaction.placeholder.datums,
      id: transaction.placeholder.id,
      votes: transaction.placeholder.votes,
      proposal_procedures: transaction.placeholder.proposal_procedures,
      current_treasury_amount: transaction.placeholder.current_treasury_amount,
      treasury_donation: transaction.placeholder.treasury_donation
    }

  statemachine.spend(threadtoken, Some(old_datum), redeemer, own_ref, transaction)
}


