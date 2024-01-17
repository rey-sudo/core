import { createRouter, createWebHistory } from "vue-router";
import storeRoute from "@/pages/store/router";

const router = createRouter({
  history: createWebHistory(),
  routes: [
    {
      path: "/",
      ...storeRoute
    },
    { path: "/:notFound(.*)", redirect: "/" },
  ],
});

export { router };
