import { useStore } from "vuex";
import { computed } from "vue";

const entryAPI = () => {
  const store = useStore();

  const userLogin = async (params) =>
    await store.dispatch("entry/userLogin", params);

  const createUser = async (params) =>
    await store.dispatch("entry/createUser", params);

  const sleep = (timeInMs) =>
    new Promise((resolve) => setTimeout(() => resolve(false), timeInMs));

  return {
    getProductData: computed(() => store.getters["dashboard/getProductData"]),
    userLogin,
    createUser,
    sleep,
  };
};

export default entryAPI;
