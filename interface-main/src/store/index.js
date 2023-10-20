import { createStore } from "vuex";
import { land } from "@/pages/land/store";
import { order } from "@/pages/order/store";
import { store } from "@/pages/store/store";

const stores = createStore({
  modules: {
    land,
    order,
    store
  },
});

export { stores };
