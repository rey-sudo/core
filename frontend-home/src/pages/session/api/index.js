import { useStore } from "vuex";
import { computed } from "vue";

const sessionAPI = () => {
  const store = useStore();

  const getOrder = async (params) =>
    await store.dispatch("session/getOrder", params);

  const startEndpoint = async (params) =>
    await store.dispatch("session/startEndpoint", params);

  return {
    getOrder,
    startEndpoint,
    getOrderData: computed(() => store.getters["session/getOrderData"]),
  };
};

export { sessionAPI };
