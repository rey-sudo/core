<template>
  <div>
    <span>{{ slot_id }} </span>
    <span>---</span>
    <span>{{ buyer_pubkeyhash }} </span>
    <div style="cursor: pointer" @click="buyProduct">send</div>
  </div>
</template>

<script>
import productAPI from "@/pages/product/api";
import { Lucid, getAddressDetails } from "lucid-cardano";
import { balanceTx } from "@/api/wallet-api";

export default {
  setup() {
    const { lockingEndpoint } = productAPI();

    return {
      lockingEndpoint,
    };
  },
  data() {
    return {
      lucid: null,
      slot_id: "",
      buyer_pubkeyhash: "",
    };
  },

  created() {
    this.$watch(
      () => this.$route.params,
      (e) => (this.slot_id = e.id),
      { immediate: true }
    )();
  },
  methods: {
    async setupLucid() {
      this.lucid = await Lucid.new();
    },
    async setupWallet() {
      const api = await window.cardano.nami.enable();

      this.lucid.selectWallet(api);
    },
    async buyProduct() {
      const addr = await this.lucid.wallet.address();
      const address = await getAddressDetails(addr);

      this.buyer_pubkeyhash = address.paymentCredential.hash;

      const params = {
        slot_id: this.slot_id,
        buyer_pubkeyhash: address.paymentCredential.hash,
      };

      this.lockingEndpoint(params)
        .then((res) => balanceTx(res.response.payload.transaction))
        .then((tx) => console.log(tx))
        .catch((err) => console.log(err));
    },
  },

  mounted() {
    this.setupLucid();

    this.setupWallet();
  },
};
</script>

<style lang="css" scoped></style>
