import { Wallet } from "@cardano-foundation/cardano-connect-with-wallet-core";

const setup = () => {
  Wallet.addEventListener("enabled", (e) => {
    console.log(e);
  });
  Wallet.addEventListener("connecting", (e) => {
    console.log(e);
  });

  Wallet.addEventListener("connected", (e) => {
    console.log(e);
  });

  Wallet.addEventListener("enabledWallet", (e) => {
    console.log(e);
  });

  Wallet.addEventListener("accountBalance", (e) => {
    console.log(e);
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
  await Wallet.connect(walletName, 'testnet', (res) => {
    console.log(res);
    window.cardano.nami.enable()
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
