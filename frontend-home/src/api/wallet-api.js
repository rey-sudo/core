import { Wallet } from "@cardano-foundation/cardano-connect-with-wallet-core";

import * as CardanoWasm from "@emurgo/cardano-serialization-lib-browser";

import { Lucid, utxoToCore } from "lucid-cardano";

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
  ]).then(async (promises) => {
    const changeAddrCbor = promises[0];

    const changeAddrBech32 = CardanoWasm.Address.from_bytes(
      fromHexString(changeAddrCbor)
    ).to_bech32();

    const utxosCbor = promises[1];

    const utxos = utxosCbor.map((cbor) =>
      CardanoWasm.TransactionUnspentOutput.from_bytes(fromHexString(cbor))
    );

    const pp = promises[2];

    const utx = CardanoWasm.Transaction.from_bytes(fromHexString(unbalancedTx));

    console.log(utx.to_json());

    await buildTx(
      { paymentAddr: changeAddrBech32 },
      utxos,
      utx.body().outputs(),
      pp,
      null,
      utx.body().inputs()
    );

    /////////////

    const transactionWitnessSet = CardanoWasm.TransactionWitnessSet.new();

    const tx = CardanoWasm.Transaction.new(
      utx.body(),
      CardanoWasm.TransactionWitnessSet.from_bytes(
        transactionWitnessSet.to_bytes()
      )
    );

    ////////////////////////////////////////////////////////

    let localWitnesses = await connectedWallet.signTx(
      Buffer.from(tx.to_bytes(), "utf8").toString("hex"),
      true
    );

    const external =
      "a10081825820a1762bfee703366b36b0c15a3ab14357cbc74a447b035609aa65e479470d0bf25840973f867b73c206118f93c14d86e91ad9ed03817be6cfe746c9d4d8c5f7f9b66795d3e35fee3ec91a1fa9ec476a8588bb2172d2d83a5224c4fc003f56d0cda402";

    ////////////////////////////////////////////////////////

    localWitnesses = CardanoWasm.TransactionWitnessSet.from_bytes(
      Buffer.from(localWitnesses, "hex")
    );

    const externalWitnesses = CardanoWasm.TransactionWitnessSet.from_bytes(
      Buffer.from(external, "hex")
    );

    const mixedVkeys = CardanoWasm.Vkeywitnesses.new();

    mixedVkeys.add(localWitnesses.vkeys().get(0));

    mixedVkeys.add(externalWitnesses.vkeys().get(0));

    transactionWitnessSet.set_vkeys(mixedVkeys);

    const finishWitnessSet = utx.witness_set();

    finishWitnessSet.set_vkeys(mixedVkeys);

    console.log("OLD SET", finishWitnessSet.to_json());

    const signedTx = CardanoWasm.Transaction.new(tx.body(), finishWitnessSet);

    console.log(signedTx.to_json());

    return connectedWallet.submitTx(signedTx);
  });
};

const buildTx = async (
  account,
  utxos,
  outputs,
  protocolParameters,
  auxiliaryData
) => {
  const txBuilderConfig = CardanoWasm.TransactionBuilderConfigBuilder.new()
    .coins_per_utxo_byte(
      CardanoWasm.BigNum.from_str(protocolParameters.coinsPerUtxoWord)
    )
    .fee_algo(
      CardanoWasm.LinearFee.new(
        CardanoWasm.BigNum.from_str(protocolParameters.linearFee.minFeeA),
        CardanoWasm.BigNum.from_str(protocolParameters.linearFee.minFeeB)
      )
    )
    .key_deposit(CardanoWasm.BigNum.from_str(protocolParameters.keyDeposit))
    .pool_deposit(CardanoWasm.BigNum.from_str(protocolParameters.poolDeposit))
    .max_tx_size(protocolParameters.maxTxSize)
    .max_value_size(protocolParameters.maxValSize)
    .build();
  //.collateral_percentage(protocolParameters.collateralPercentage)
  //.max_collateral_inputs(protocolParameters.maxCollateralInputs)
  //.ex_unit_prices(CardanoWasm.ExUnitPrices)

  const txBuilder = CardanoWasm.TransactionBuilder.new(txBuilderConfig);

  txBuilder.add_output(outputs.get(0));

  if (auxiliaryData) txBuilder.set_auxiliary_data(auxiliaryData);

  const utxosCore = CardanoWasm.TransactionUnspentOutputs.new();

  utxos.forEach((utxo) => utxosCore.add(utxo));

  const utxoi = {
    txHash: "919818c757b8242ffc60ed2ca4bdf4d2c0dea87203075143b1565978197dac3b",
    outputIndex: 0,
    assets: {
      lovelace: 10000000n,
      "54a29c2626156de3af97cdead84264aaf0805857cc5c026af077fc3b746872656164746f6b656e":
        1n,
    },
    address: "addr_test1wp4ep7h3mw4fvse8v8lmafzjpettgfm972r783mzlcemzrg5avvkf",
    datumHash:
      "1cc953c6981e5e524f90f459f28847ab24455c9ee3ae7c8916d4889ceb2d8a11",
    datum:
      "d8799f00581c424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bff",
    scriptRef: null,
  };

  try {
    utxosCore.add(
      CardanoWasm.TransactionUnspentOutput.from_hex(
        Buffer.from(utxoToCore(utxoi).to_bytes()).toString("hex")
      )
    );
  } catch (err) {
    console.error(err);
  }

  txBuilder.add_inputs_from(
    utxosCore,
    CardanoWasm.CoinSelectionStrategyCIP2.LargestFirstMultiAsset
  );

  txBuilder.add_change_if_needed(
    CardanoWasm.Address.from_bech32(account.paymentAddr)
  );

  return txBuilder.build();
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
