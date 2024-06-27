<template>
  <main>
    <router-view />
  </main>
</template>

<script>
import { headerAPI } from "@/components/header/composable/header-api";
import { walletClient } from "@/api/wallet-api";

export default {
  name: "App",

  setup() {
    const { currentSeller } = headerAPI();
    const { startWalletService, stopWalletService } = walletClient();

    return {
      currentSeller,
      stopWalletService,
      startWalletService,
    };
  },

  mounted() {
    this.currentSeller().catch((err) => console.error(err));

    this.startWalletService();
  },

  beforeUnmount() {
    this.stopWalletService();
  },
};
</script>

<style src="./style/global.css" />
