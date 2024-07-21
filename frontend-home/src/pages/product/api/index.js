import { useStore } from "vuex";
import { computed } from "vue";

const productAPI = () => {
  const store = useStore();

  const lockingEndpoint = async (params) =>
    await store.dispatch("product/lockingEndpoint", params);

  const getOrders = async (params) =>
    await store.dispatch("product/getOrders", params);

  const getProduct = async (params) =>
    await store.dispatch("product/getProduct", params);

  const sleep = (timeInMs) =>
    new Promise((resolve) => setTimeout(() => resolve(false), timeInMs));

  return {
    lockingEndpoint,
    getProduct,
    getOrders,
    getProductData: computed(() => store.getters["product/getProductData"]),
    sleep,
  };
};

export default productAPI;
