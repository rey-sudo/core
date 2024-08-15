import { useStore } from "vuex";
import { computed } from "vue";

const sessionAPI = () => {
  const store = useStore();

  const getOrder = async (params) =>
    await store.dispatch("session/getOrder", params);

  const deploy = async (params) =>
    await store.dispatch("session/deploy", params);

  const deployTx = async (params) =>
    await store.dispatch("session/deployTx", params);

  return {
    getOrder,
    deploy,
    deployTx,
    getOrderData: computed(() => store.getters["session/getOrderData"]),
  };
};

export { sessionAPI };
