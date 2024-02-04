import { createRouter, createWebHistory } from "vue-router";
import dashboardRoute from "@/pages/dashboard/router";

const router = createRouter({
  history: createWebHistory(),
  routes: [
    {
      path: "/dashboard",
      ...dashboardRoute
    },
    { path: "/:notFound(.*)", redirect: "/" },
  ],
});

export { router };
