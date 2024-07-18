import { useStore } from "vuex";

const productAPI = () => {
  const store = useStore();

  const lockingEndpoint = async (params) =>
    await store.dispatch("product/lockingEndpoint", params);

  const  getProduct = async (params) =>
    await store.dispatch("product/getProduct", params);
 
  const sleep = (timeInMs) =>
    new Promise((resolve) => setTimeout(() => resolve(false), timeInMs));

  return {
    lockingEndpoint,
    getProduct,
    sleep,
  };
};

export default productAPI;
