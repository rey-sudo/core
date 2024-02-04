import { createStore } from "vuex";
import { store } from "@/pages/store/store";

const stores = createStore({
  modules: {
    store
  },
});

export { stores };
