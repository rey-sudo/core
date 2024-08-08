import { Wallet } from "@cardano-foundation/cardano-connect-with-wallet-core";

import * as CardanoWasm from "@emurgo/cardano-serialization-lib-browser";

import { Lucid } from "lucid-cardano";

const Buffer = require("buffer/").Buffer;

let connectedWallet = null;

const lucidClient = await Lucid.new();

const walletEnabledEvent = new CustomEvent("walletEnabledEvent", {
  detail: {
    payload: "wallet enabled",
  },
});

const walletClient = () => {
  return {
    startWalletService,
    stopWalletService,
    connect,
    reconnect,
    getWallet,
  };
};

const getWallet = async () => {
  if (!connectedWallet) {
    await reconnect();
  }

  return connectedWallet;
};

const connect = async (walletName) => {
  await Wallet.connect(walletName, "testnet", async () => {
    connectedWallet = await window.cardano[walletName].enable();

    localStorage.setItem("pairfy-wallet", walletName);

    console.log("CONNECTED " + walletName);
  });
};

const reconnect = async () => {
  const walletName = localStorage.getItem("pairfy-wallet");

  if (walletName !== null) {
    await connect(walletName);
    console.log("RECONNECTED " + walletName);
  } else {
    return false;
  }
};

const getAddress = async () => {
  if (!connectedWallet) {
    await reconnect();
  }

  const address = await connectedWallet.getUsedAddresses();

  return address[0];
};

const getMessage = () => {
  const message = "PLEASE SIGN TO AUTHENTICATE IN PAIRFY";

  return Buffer.from(message, "utf8").toString("hex");
};

const signMessage = async () => {
  if (!connectedWallet) {
    await reconnect();
  }

  return await connectedWallet.signData(await getAddress(), getMessage());
};

const startWalletService = async () => {
  Wallet.addEventListener("enabledWallet", async (walletName) => {
    const isEnabled = await window.cardano[walletName].isEnabled();

    if (isEnabled) {
      localStorage.setItem("pairfy-wallet", walletName);

      window.dispatchEvent(walletEnabledEvent);

      console.info("ENABLED_WALLET", walletName);
    }
  });

  Wallet.startInjectWalletListener();

  await reconnect();
};

const stopWalletService = () => {
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

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

const balanceTx = (unbalancedTx) => {
  return Promise.all([
    connectedWallet.getChangeAddress(),
    connectedWallet.getUtxos(),
    fetchProtocolParameters(),
  ]).then(async () => {


    const utx = CardanoWasm.Transaction.from_bytes(fromHexString(unbalancedTx));



    /////////////

    const tx = CardanoWasm.Transaction.new(utx.body(), utx.witness_set());

    console.log("BODY", tx.to_json());

    let txVkeyWitnesses = await connectedWallet.signTx(
      Buffer.from(tx.to_bytes(), "utf8").toString("hex"),
      true
    );

    txVkeyWitnesses = CardanoWasm.TransactionWitnessSet.from_bytes(
      Buffer.from(txVkeyWitnesses, "hex")
    );

    const newTransactionWitnessSet = utx.witness_set();

    newTransactionWitnessSet.set_vkeys(txVkeyWitnesses.vkeys());

    const signedTx = CardanoWasm.Transaction.new(
      tx.body(),
      newTransactionWitnessSet
    );

    console.log("NEWBODY", signedTx.to_json());

    return connectedWallet.submitTx(
      Buffer.from(signedTx.to_bytes(), "utf8").toString("hex")
    );
  });
};


const fetchProtocolParameters = () => {
  return fetch("https://cardano-preview.blockfrost.io/api/v0/blocks/latest", {
    headers: {
      project_id: "previewXgODba40jVJAs1QgKTBOAuwhvNFHHMVo",
    },
  })
    .then((res) => res.json())
    .then((latestBlock) => {
      return fetch(
        `https://cardano-preview.blockfrost.io/api/v0/epochs/${latestBlock.epoch}/parameters`,
        {
          headers: {
            project_id: "previewXgODba40jVJAs1QgKTBOAuwhvNFHHMVo",
          },
        }
      )
        .then((res) => res.json())
        .then((p) => {
          return {
            linearFee: {
              minFeeA: p.min_fee_a.toString(),
              minFeeB: p.min_fee_b.toString(),
            },
            minUtxo: "1000000", //p.min_utxo, minUTxOValue protocol paramter has been removed since Alonzo HF. Calulation of minADA works differently now, but 1 minADA still sufficient for now
            poolDeposit: p.pool_deposit,
            keyDeposit: p.key_deposit,
            coinsPerUtxoWord: "34482",
            maxValSize: 5000,
            priceMem: 5.77e-2,
            priceStep: 7.21e-5,
            maxTxSize: parseInt(p.max_tx_size),
            slot: parseInt(latestBlock.slot),
          };
        });
    });
};

const fromHexString = (hexString) =>
  new Uint8Array(hexString.match(/.{1,2}/g).map((byte) => parseInt(byte, 16)));

// padd with leading 0 if <16
//const i2hex = (i) => ("0" + i.toString(16)).slice(-2);

//const toHexString = (uint8) => Array.from(uint8).map(i2hex).join("");

export {
  walletClient,
  CardanoWasm,
  balanceTx,
  lucidClient,
  signMessage,
  getAddress,
  getMessage,
};
