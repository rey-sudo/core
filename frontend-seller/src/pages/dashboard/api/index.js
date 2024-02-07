import { useStore } from "vuex";
import { computed } from "vue";

const dashboardAPI = () => {
  const store = useStore();

  const fetchProductData = async (params) =>
    await store.dispatch("dashboard/fetchProductData", params);

  const createProduct = async (params) =>
    await store.dispatch("dashboard/createProduct", params);

  const sleep = (timeInMs) =>
    new Promise((resolve) => setTimeout(() => resolve(false), timeInMs));

  return {
    getProductData: computed(() => store.getters["dashboard/getProductData"]),
    fetchProductData,
    createProduct,
    sleep,
  };
};

export default dashboardAPI;
