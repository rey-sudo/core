import { useStore } from "vuex";
import { computed } from "vue";

const landAPI = () => {
  const store = useStore();

  const __action__viewPaymentModal = async (params) =>
    await store.dispatch("land/__action__viewPaymentModal", params);

  const sleep = (timeInMs) =>
    new Promise((resolve) => setTimeout(() => resolve(false), timeInMs));

  return {
    getter__productData: computed(
      () => store.getters["land/getter__productData"]
    ),
    getter__viewPaymentModal: computed(
      () => store.getters["land/getter__viewPaymentModal"]
    ),

    __action__viewPaymentModal,
    sleep,
  };
};

export default landAPI;
