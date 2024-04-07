<template>
  <div class="product">
    <MainHeader />

  </div>
</template>

<script>
import productAPI from "@/pages/product/api";
import MainHeader from "@/components/MainHeader.vue";
import { Lucid, getAddressDetails } from "lucid-cardano";
import { balanceTx } from "@/api/wallet-api";

export default {
  components: {
    MainHeader,
  },
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
      tx1: "84a400800181a300581d70db457aaf29e54e6bdcb4ce9645982e3f19ac3f23187e91ce2a1f4b20011a01312d04028201d8185841d8799f004777616974696e67d87980d87980d87980581c4068ce72a0f73e850f19899a10b82ec534a55a6d860e5c5267dca2b9d87a801a01c9c3841a01312d04ff02000e81581c4068ce72a0f73e850f19899a10b82ec534a55a6d860e5c5267dca2b9a0f5f6",
      tx2: "",
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

    async buyProduct1(e) {
      const addr = await this.lucid.wallet.address();
      const address = await getAddressDetails(addr);

      this.buyer_pubkeyhash = address.paymentCredential.hash;

      if (e === "1") {
        await balanceTx(this.tx1)
          .then((tx) => console.log(tx))
          .catch((err) => console.log(err));
      }

      if (e === "2") {
        await balanceTx(this.tx2)
          .then((tx) => console.log(tx))
          .catch((err) => console.log(err));
      }
    },
  },

  mounted() {
    this.setupLucid();

    this.setupWallet();
  },
};
</script>

<style lang="css" scoped>
.product{

}

</style>
