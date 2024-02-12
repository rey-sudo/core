import { HOST } from "@/api";

const eventMachine = {
  setup: () => {
    const SSEurl = HOST + "/api/product/get-events";

    const eventSource = new EventSource(SSEurl, { withCredentials: true });

    eventSource.onopen = function () {
      console.log("SSE connection opened.");
    };

    eventSource.onerror = function (error) {
      console.error("SSE connection error:", error);
    };

    eventSource.onmessage = function (event) {
      const datum = JSON.parse(event.data);

      const sendEvent = new CustomEvent("globalMessage", {
        detail: {
          data: datum,
        },
      });

      document.dispatchEvent(sendEvent);
    };
  },
};

export { eventMachine };
