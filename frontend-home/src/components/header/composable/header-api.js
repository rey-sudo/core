import { useStore } from "vuex";
import { computed } from "vue";

const headerAPI = () => {
  const store = useStore();

  const currentSeller = async (params) =>
    await store.dispatch("header/currentSeller", params);

  const currentUser = async (params) =>
    await store.dispatch("header/currentUser", params);

  const setupWallet = async (params) =>
    await store.dispatch("header/setupWallet", params);

  const connectWallet = async (params) =>
    await store.dispatch("header/connectWallet", params);

  const loginSeller = async (params) =>
    await store.dispatch("header/loginSeller", params);

  const loginUser = async (params) =>
    await store.dispatch("header/loginUser", params);

  const logoutSeller = async (params) =>
    await store.dispatch("header/logoutSeller", params);

  const setupLucid = async (data) =>
    await store.dispatch("header/setupLucid", data);

  const startTx = async (data) => await store.dispatch("header/startTx", data);

  return {
    currentSeller,
    setupWallet,
    currentUser,
    loginSeller,
    logoutSeller,
    loginUser,
    startTx,
    setupLucid,
    connectWallet,
    getCurrentSeller: computed(() => store.getters["header/getCurrentSeller"]),
    getCurrentUser: computed(() => store.getters["header/getCurrentUser"]),
    getSetupWallet: computed(() => store.getters["header/getSetupWallet"]),
    getLucid: computed(() => store.getters["header/getLucid"]),
  };
};

export default headerAPI;
