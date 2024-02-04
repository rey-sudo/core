import { createRouter, createWebHistory } from "vue-router";
import storeRoute from "@/pages/store/router";
import dashboardRoute from "@/pages/dashboard/router";

const router = createRouter({
  history: createWebHistory(),
  routes: [
    {
      path: "/",
      ...storeRoute
    },
    {
      path: "/dashboard",
      ...dashboardRoute
    },
    { path: "/:notFound(.*)", redirect: "/" },
  ],
});

export { router };
