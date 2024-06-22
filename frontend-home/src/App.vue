<template>
  <main>
    <router-view />
  </main>
</template>

<script>
import { headerAPI } from "@/components/header/composable/header-api";

export default {
  name: "App",

  setup() {
    const { currentSeller } = headerAPI();

    return {
      currentSeller,
    };
  },

  mounted() {
    const { currentSeller } = headerAPI();
    currentSeller().catch((err) => console.error(err));
  },
};
</script>

<style src="./style/global.css" />
