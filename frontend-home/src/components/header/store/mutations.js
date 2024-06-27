const currentSeller = (state, data) => {
  state.sellerData = data;
};

const setupWallet = (state, data) => {
  state.displaySetupWallet = data;
};

const connectWallet = (state, data) => {
  state.walletConnected = data.value;
  state.walletName = data.name;
};

export { connectWallet, setupWallet, currentSeller };
