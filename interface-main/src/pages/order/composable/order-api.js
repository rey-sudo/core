import { useStore } from "vuex";
import { computed } from 'vue';

const orderAPI = () => {
  const store = useStore();

  const action__getOrderData = async (params) =>
    await store.dispatch("order/action__getOrderData", params);

  const sleep = (timeInMs) =>
    new Promise((resolve) => setTimeout(() => resolve(false), timeInMs));

  return {
    action__getOrderData,
    getter__orderData: computed(
      () => store.getters["order/getter__orderData"]
    ),
    sleep,
  };
};

export default orderAPI;
