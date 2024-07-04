<template>
  <main>
    <router-view />
  </main>
</template>

<script>
import { headerAPI } from "@/components/header/composable/header-api";
import { walletClient } from "@/api/wallet-api";
import { Lucid } from "lucid-cardano";

export default {
  name: "App",

  setup() {
    const { currentSeller, setupLucid } = headerAPI();
    const { startWalletService, stopWalletService } = walletClient();

    currentSeller()
      .then(() => console.info("SELLER_LOGGED"))
      .catch((err) => console.error(err));

    startWalletService()
      .then(() => console.info("WALLET_SERVICE"))
      .catch((err) => console.error(err));

    setupLucid(Lucid.new())
      .then(() => console.info("LUCID_SERVICE"))
      .catch((err) => console.error(err));

    return {
      currentSeller,
      stopWalletService,
      startWalletService,
    };
  },

  beforeUnmount() {
    this.stopWalletService();
  },
};
</script>

<style src="./style/global.css" />
