const getCurrentSeller = (state) => {
  return state.sellerData;
};

const getSetupWallet = (state) => {
  return state.displaySetupWallet;
};

const getLucid = (state) => {
  return state.lucidClient;
};

export { getCurrentSeller, getSetupWallet, getLucid };
