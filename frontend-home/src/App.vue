<template>
  <main>
    <router-view />
  </main>
</template>

<script>
import { headerAPI } from "@/components/header/composable/header-api";
import { walletAPI } from "@/api/wallet-api";

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

    const { stop, setup } = walletAPI();
    stop();
    setup();
  },
};
</script>

<style src="./style/global.css" />
