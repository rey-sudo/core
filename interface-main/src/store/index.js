import { createStore } from "vuex";
import { land } from "@/pages/land/store";
import { order } from "@/pages/order/store";

const store = createStore({
  modules: {
    land,
    order
  },
});

export { store };
