const currentSeller = (state, data) => {
  state.sellerData = data;
};

const setupWallet = (state, data) => {
  state.displaySetupWallet = data;
};

export { setupWallet, currentSeller };
