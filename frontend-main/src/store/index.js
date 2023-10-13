import { createStore } from "vuex";
import { land } from "@/pages/land/store";

const store = createStore({
  modules: {
    land,
  },
});

export { store };
