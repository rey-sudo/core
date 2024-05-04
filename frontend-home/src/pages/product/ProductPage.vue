<template>
  <div class="product">
    <MainHeader />
    <MobileHeader />
    <div class="product-wrap">
      <div class="product-wrap-top">
        <div class="bread">
          <Breadcrumb :home="home" :model="breadItems">
            <template #separator>
              <div class="arrow" />
            </template>
          </Breadcrumb>
        </div>

        <HeadView />
        <MobileHeadView />
      </div>

      <div class="product-wrap-body"></div>
    </div>
  </div>
</template>

<script>
import productAPI from "@/pages/product/api";
import MainHeader from "@/components/MainHeader.vue";
import MobileHeader from "@/components/MobileHeader.vue";
import HeadView from "@/pages/product/components/HeadView.vue";
import MobileHeadView from "@/pages/product/components/MobileHeadView.vue";

import { ref } from "vue";
import { Lucid, getAddressDetails } from "lucid-cardano";
import { balanceTx } from "@/api/wallet-api";

export default {
  components: {
    MainHeader,
    MobileHeader,
    HeadView,
    MobileHeadView,
  },
  setup() {
    const { lockingEndpoint } = productAPI();

    const product = ref({
      rating_count: 4.8,
      review_count: 756,
    });

    const breadItems = ref([
      { label: "Electronics" },
      { label: "TV & Accessories" },
      { label: "TVs" },
    ]);

    return {
      product,
      breadItems,
      lockingEndpoint,
    };
  },
  data() {
    return {
      lucid: null,
      slot_id: "",
      buyer_pubkeyhash: "",
      tx1: "84a400800181a300581d70c11355d423aed1273fa29eb0aaae09f63f025890f03fedd6ade0b4f5011a01312d0b028201d8185841d8799f004777616974696e67d87980d87980d87980581c4068ce72a0f73e850f19899a10b82ec534a55a6d860e5c5267dca2b9d87a801a01c9c38b1a01312d0bff02000e81581c4068ce72a0f73e850f19899a10b82ec534a55a6d860e5c5267dca2b9a0f5f6",
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
    isGalleryImage(index) {
      return this.galleryImageIndex === index;
    },
    setupData() {
      this.galleryImage[0] = this.images[0];
    },
    changeGalleryImage(index) {
      this.galleryImageIndex = index;
      this.galleryImage[0] = this.images[this.galleryImageIndex];
    },
    async setupLucid() {
      this.lucid = await Lucid.new();
    },
    async setupWallet() {
      try {
        const api = await window.cardano.nami.enable();

        this.lucid.selectWallet(api);
      } catch (err) {
        console.error(err);
      }
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
.arrow::before {
  border-bottom: 4px solid #0000;
  border-left: 4px solid var(--text-b);
  border-top: 4px solid #0000;
  content: " ";
  display: inline-block;
  align-items: center;
  height: 0;
  margin: 0 2px;
  width: 0;
}

.product-wrap {
  width: 80%;
}

.bread {
  display: flex;
  justify-content: flex-start;
  width: inherit;
  padding: 2rem 0;
}

.product {
  display: flex;
  justify-content: center;
  min-height: 100vh;
  width: 100%;
  flex-direction: column;
  align-items: center;
}

.product-wrap-top {
  min-height: 150vh;
  margin-top: 150px;
  display: flex;
  flex-wrap: wrap;
  flex-direction: column;
}

@media only screen and (max-width: 767px) {
  .product-wrap {
    width: 100%;
  }

  .bread {
    padding: 1rem;
  }

  .product-wrap-top{
    margin-top: 100px;
  }
}

/* Tablets and small deskbanners */
@media only screen and (min-width: 768px) and (max-width: 991px) {
  /* CSS rules for tablets and small deskbanners */
}

/* Medium deskbanners */
@media only screen and (min-width: 992px) and (max-width: 1199px) {
  /* CSS rules for medium deskbanners */
}

/* Large deskbanners and widescreen monitors */
@media only screen and (min-width: 1200px) {
  /* CSS rules for large deskbanners and widescreen monitors */
}
</style>
