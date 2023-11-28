import { createStore } from "vuex";
import { order } from "@/pages/order/store";
import { store } from "@/pages/store/store";

const stores = createStore({
  modules: {
    order,
    store
  },
});

export { stores };
