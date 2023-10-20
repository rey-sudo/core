import { createRouter, createWebHistory } from "vue-router";
import landRoute from "@/pages/land/router";
import storeRoute from "@/pages/store/router";
import orderRoute from '@/pages/order/router'

const router = createRouter({
  history: createWebHistory(),
  routes: [
    {
      path: "/order/:pid/:name?",
      ...orderRoute
    },
    {
      path: "/p/:pid/:name?",
      ...landRoute
    },
    {
      path: "/",
      ...storeRoute
    },
    { path: "/:notFound(.*)", redirect: "/" },
  ],
});

export { router };
