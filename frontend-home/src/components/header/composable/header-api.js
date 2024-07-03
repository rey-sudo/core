import { useStore } from "vuex";
import { computed } from "vue";

const headerAPI = () => {
  const store = useStore();

  const currentSeller = async (params) =>
    await store.dispatch("header/currentSeller", params);

  const setupWallet = async (params) =>
    await store.dispatch("header/setupWallet", params);

  const connectWallet = async (params) =>
    await store.dispatch("header/connectWallet", params);

  const loginSeller = async (params) =>
    await store.dispatch("header/loginSeller", params);

  const logoutSeller = async (params) =>
    await store.dispatch("header/logoutSeller", params);

  return {
    currentSeller,
    setupWallet,
    loginSeller,
    logoutSeller,
    connectWallet,
    getCurrentSeller: computed(() => store.getters["header/getCurrentSeller"]),
    getSetupWallet: computed(() => store.getters["header/getSetupWallet"]),
  };
};

export { headerAPI };
