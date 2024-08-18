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

  const cancel = async (params) =>
    await store.dispatch("session/cancel", params);

  const cancelTx = async (params) =>
    await store.dispatch("session/cancelTx", params);

  return {
    getOrder,
    deploy,
    deployTx,
    cancel,
    cancelTx,
    getOrderData: computed(() => store.getters["session/getOrderData"]),
  };
};

export { sessionAPI };
