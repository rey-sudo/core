import { useStore } from "vuex";
import { computed } from "vue";

const landAPI = () => {
  const store = useStore();

  const action__viewPaymentModal = async (params) =>
    await store.dispatch("land/action__viewPaymentModal", params);

  const action__viewPaymentModalMobile = async (params) =>
    await store.dispatch("land/action__viewPaymentModalMobile", params);

  const action__getProductData = async (params) =>
    await store.dispatch("land/action__getProductData", params);

  const action__createOrder = async (params) =>
    await store.dispatch("land/action__createOrder", params);

  const formatPrice = (num) => {
    const price = num || 0;

    const formattedPrice = price.toLocaleString("en-US", {
      style: "currency",
      currency: "COP",
    });

    return formattedPrice;
  };

  const sleep = (timeInMs) =>
    new Promise((resolve) => setTimeout(() => resolve(false), timeInMs));

  return {
    getter__productData: computed(
      () => store.getters["land/getter__productData"]
    ),
    getter__viewPaymentModal: computed(
      () => store.getters["land/getter__viewPaymentModal"]
    ),
    getter__viewPaymentModalMobile: computed(
      () => store.getters["land/getter__viewPaymentModalMobile"]
    ),
    action__viewPaymentModal,
    action__viewPaymentModalMobile,
    action__getProductData,
    action__createOrder,
    formatPrice,
    sleep,
  };
};

export default landAPI;
