import { useStore } from "vuex";
import { computed } from "vue";

const sellerAPI = () => {
  const store = useStore();

  const currentSeller = async (params) =>
    await store.dispatch("seller/currentSeller", params);

  return {
    currentSeller,
    getCurrentSeller: computed(() => store.getters["seller/getCurrentSeller"]),
  };
};

export { sellerAPI };
