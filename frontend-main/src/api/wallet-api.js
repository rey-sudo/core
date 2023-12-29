import { Wallet } from "@cardano-foundation/cardano-connect-with-wallet-core";

const setup = () => {
  Wallet.addEventListener("enabled", (e) => {
    console.log(e);
  });
  Wallet.addEventListener("connecting", (e) => {
    console.log(e);
  });

  Wallet.addEventListener("connected", async (e) => {
    console.log(e);
  });

  Wallet.addEventListener("enabledWallet", (e) => {
    console.log(e);
  });

  Wallet.addEventListener("accountBalance", async (e) => {
    console.log(e);
    const tx =
      "84a300800181a300581d7055743d5cfd66af33d0891dc9dba441bce20d632b0fe81b0b6cfe483e011a004c4b40028201d818585cd8799f004777616974696e67d87980d87980d87980581c484ebc54b4112e54e1f7524dbdc6bb42635648a06c297e584592e80b581c3f2ec097f77e4254df012d5d4d4b45e48459c6ec5795e92df30f2dbc1a009896801a004c4b40ff0200a0f5f6";

    const result = await window.cardano.eternl.enable();

    await result.signTx(tx, false).catch((err) => console.log(err));
    console.log(result);
  });

  Wallet.startInjectWalletListener();
};

const stop = () => {
  Wallet.disconnect();

  Wallet.removeEventListener("enabled", (e) => {
    console.log(e);
  });
  Wallet.removeEventListener("connecting", (e) => {
    console.log(e);
  });

  Wallet.removeEventListener("connected", (e) => {
    console.log(e);
  });

  Wallet.removeEventListener("enabledWallet", (e) => {
    console.log(e);
  });

  Wallet.removeEventListener("accountBalance", (e) => {
    console.log(e);
  });

  Wallet.stopInjectWalletListener();
};

const connect = async (walletName) => {
  await Wallet.connect(walletName, "testnet", (res) => {
    console.log(res);
  });
};

const walletAPI = () => {
  return {
    setup,
    stop,
    connect,
  };
};
export default walletAPI;
