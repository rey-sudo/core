<template>
  <main>
    <router-view />
  </main>
</template>

<script>
import entryAPI from "@/pages/entry/api";
import { HOST } from "./api";

export default {
  name: "App",
  setup() {
    const { getUser } = entryAPI();
    return {
      getUser,
    };
  },
  mounted() {
    this.getUser()
      .then(() => {
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
        };
      })
      .catch((err) => {
        console.error(err);
      });
  },
};
</script>

<style src="./style/global.css" />
