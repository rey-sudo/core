import { HOST } from "@/api";
import dashboardAPI from "@/pages/dashboard/api";

const eventMachine = {
  run: () => {
    const SSEurl = HOST + "/api/product/get-events";

    const eventSource = new EventSource(SSEurl, { withCredentials: true });

    eventSource.onopen = function () {
      console.log("SSE connection opened.");
    };

    eventSource.onerror = function (error) {
      console.error("SSE connection error:", error);
    };

    eventSource.onmessage = function (event) {
      console.log(event);
      const { getProducts } = dashboardAPI();
      getProducts().catch((err) => console.log(err));
    };
  },
};

export default eventMachine;
