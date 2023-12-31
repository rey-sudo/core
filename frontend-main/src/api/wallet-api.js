import { Wallet } from "@cardano-foundation/cardano-connect-with-wallet-core";

import * as CardanoWasm from "@emurgo/cardano-serialization-lib-asmjs";

const walletAPI = () => {
  return {
    setup,
    stop,
    connect,
  };
};

const connect = async (walletName) => {
  await Wallet.connect(walletName, "testnet", () => {
    console.log("connect call");
    window.cardano.enable();
  });
};

const setup = () => {
  Wallet.addEventListener("enabled", async (e) => {
    console.log("enabled", e, await window.cardano.isEnabled());
  });
  Wallet.addEventListener("connecting", (e) => {
    console.log("connecting", e);
  });

  Wallet.addEventListener("connected", (e) => {
    console.log("connected", e);
  });

  Wallet.addEventListener("enabledWallet", async (e) => {
    console.log("enabledW", e, await window.cardano.isEnabled());
  });

  Wallet.addEventListener("accountBalance", (e) => {
    console.log("balance", e);
  });

  Wallet.startInjectWalletListener();
};

const stop = () => {
  Wallet.disconnect();

  Wallet.removeEventListener("enabled", (e) => {
    console.log("enabled", e);
  });
  Wallet.removeEventListener("connecting", (e) => {
    console.log("connecting", e);
  });

  Wallet.removeEventListener("connected", (e) => {
    console.log("connected", e);
  });

  Wallet.removeEventListener("enabledWallet", (e) => {
    console.log("enabledw", e);
  });

  Wallet.removeEventListener("accountBalance", (e) => {
    console.log("balance", e);
  });

  Wallet.stopInjectWalletListener();
};


const balanceTx = (unbalancedTx) => {
  return Promise.all([
    window.cardano.getChangeAddress(),
    window.cardano.getUtxos(),
    fetchProtocolParameters()
  ])
  .then(promises => {
    const changeAddrCbor = promises[0]

    const changeAddrBech32 = CardanoWasm.Address.from_bytes(fromHexString(changeAddrCbor)).to_bech32()

    const utxosCbor = promises[1]
    const utxos = utxosCbor.map(cbor => CardanoWasm.TransactionUnspentOutput.from_bytes(fromHexString(cbor)))

    const pp = promises[2]

    const tx = CardanoWasm.Transaction.from_bytes(fromHexString(unbalancedTx))
    
    return window.cardano.buildTx({ 'paymentAddr': changeAddrBech32 }, utxos, tx.body().outputs(), pp)
  })
  .then(btx => toHexString(btx.to_bytes()))
}


const fetchProtocolParameters = () => {
  return fetch('https://cardano-testnet.blockfrost.io/api/v0/blocks/latest', {
      headers: {
        'project_id': 'previewXgODba40jVJAs1QgKTBOAuwhvNFHHMVo'
      },
    })
    .then(res => res.json())
    .then(latestBlock => {
      return fetch(`https://cardano-testnet.blockfrost.io/api/v0/epochs/${latestBlock.epoch}/parameters`, {
          headers: {
            'project_id': 'previewXgODba40jVJAs1QgKTBOAuwhvNFHHMVo'
          },
        })
        .then(res => res.json())
        .then(p => {
          return {
            linearFee: {
              minFeeA: p.min_fee_a.toString(),
              minFeeB: p.min_fee_b.toString(),
            },
            minUtxo: '1000000', //p.min_utxo, minUTxOValue protocol paramter has been removed since Alonzo HF. Calulation of minADA works differently now, but 1 minADA still sufficient for now
            poolDeposit: p.pool_deposit,
            keyDeposit: p.key_deposit,
            coinsPerUtxoWord: '34482',
            maxValSize: 5000,
            priceMem: 5.77e-2,
            priceStep: 7.21e-5,
            maxTxSize: parseInt(p.max_tx_size),
            slot: parseInt(latestBlock.slot),
          };
        })
    })
}



const fromHexString = hexString =>
  new Uint8Array(hexString.match(/.{1,2}/g).map(byte => parseInt(byte, 16)));


// padd with leading 0 if <16
const i2hex = i => ('0' + i.toString(16)).slice(-2)


const toHexString = uint8 => Array.from(uint8).map(i2hex).join('');



export { walletAPI, CardanoWasm, balanceTx};
