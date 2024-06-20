<template>
  <main>
    <router-view />
  </main>
</template>

<script>
import { sellerAPI } from "@/components/header/composable/seller";

export default {
  name: "App",

  setup() {
    const { currentSeller } = sellerAPI();

    return { currentSeller };
  },

  mounted() {
    const { currentSeller } = sellerAPI();
    currentSeller().catch((err) => console.error(err)); 
  },
};
</script>

<style src="./style/global.css" />
