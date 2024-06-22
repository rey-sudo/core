const getCurrentSeller = (state) => {
  return state.sellerData;
};

const getSetupWallet = (state) => {
  return state.displaySetupWallet;
};

export { getCurrentSeller, getSetupWallet };
