import { useStore } from "vuex";
import { computed } from "vue";

const dashboardAPI = () => {
  const store = useStore();

  const fetchProductData = async (params) =>
    await store.dispatch("dashboard/fetchProductData", params);

  const createProduct = async (params) =>
    await store.dispatch("dashboard/createProduct", params);

  const getProducts = async (params) =>
    await store.dispatch("dashboard/getProducts", params);
  

  const sleep = (timeInMs) =>
    new Promise((resolve) => setTimeout(() => resolve(false), timeInMs));

  return {
    getProductsData: computed(() => store.getters["dashboard/getProductsData"]),
    fetchProductData,
    createProduct,
    getProducts,
    sleep,
  };
};

export default dashboardAPI;
