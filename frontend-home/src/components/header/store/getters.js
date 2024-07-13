const getCurrentSeller = (state) => {
  return state.sellerData;
};

const getCurrentUser = (state) => {
  return state.userData;
};

const getSetupWallet = (state) => {
  return state.displaySetupWallet;
};

const getLucid = (state) => {
  return state.lucidClient;
};

export { getCurrentUser, getCurrentSeller, getSetupWallet, getLucid };
