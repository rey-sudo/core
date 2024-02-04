import { useStore } from "vuex";
import { computed } from "vue";

const storeAPI = () => {
  const store = useStore();


  const action__getAllProducts = async (params) =>
    await store.dispatch("store/action__getAllProducts", params);

  const sleep = (timeInMs) =>
    new Promise((resolve) => setTimeout(() => resolve(false), timeInMs));

  return {
    getter__allProducts: computed(
      () => store.getters["store/getter__allProducts"]
    ),
    action__getAllProducts,
    sleep,
  };
};

export default storeAPI;
