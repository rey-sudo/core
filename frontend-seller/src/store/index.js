import { createStore } from "vuex";
import { dashboard } from "@/pages/dashboard/store";

const stores = createStore({
  modules: {
    dashboard
  },
});

export { stores };
