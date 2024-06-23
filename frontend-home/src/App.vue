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
    const { startWalletService, stopWalletService } = walletAPI();

    return {
      currentSeller,
      stopWalletService,
      startWalletService,
    };
  },

  mounted() {
    const { currentSeller } = headerAPI();
    currentSeller().catch((err) => console.error(err));

    this.startWalletService();
  },

  beforeUnmount() {
    this.stopWalletService();
  },
};
</script>

<style src="./style/global.css" />
