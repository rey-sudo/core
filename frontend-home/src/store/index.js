import { createStore } from "vuex";
import { home } from "@/pages/home/store";
import { product } from "@/pages/product/store";
import { seller } from "@/components/store";
import { session } from "@/pages/session/store";


const stores = createStore({
  modules: {
    home,
    product,
    seller,
    session
  },
});

export { stores };
