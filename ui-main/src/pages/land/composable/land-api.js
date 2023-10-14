import { useStore } from "vuex";
import { computed } from "vue";

const landAPI = () => {
  const store = useStore();

  const action__viewPaymentModal = async (params) =>
    await store.dispatch("land/action__viewPaymentModal", params);

  const action__getProductData = async (params) =>
    await store.dispatch("land/action__getProductData", params);

  const sleep = (timeInMs) =>
    new Promise((resolve) => setTimeout(() => resolve(false), timeInMs));

  return {
    getter__productData: computed(
      () => store.getters["land/getter__productData"]
    ),
    getter__viewPaymentModal: computed(
      () => store.getters["land/getter__viewPaymentModal"]
    ),
    action__viewPaymentModal,
    action__getProductData,
    sleep,
  };
};

export default landAPI;
