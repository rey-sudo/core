<template>
  <div class="product">
    <MainHeader />
    <div class="product-wrap">
      <div class="product-wrap-top">
        <div class="product-wrap-top-left">
          <Galleria
            :value="images"
            :responsiveOptions="responsiveOptions"
            :numVisible="5"
            :circular="true"
            containerStyle="max-width: 80%"
            :showItemNavigators="true"
            :showThumbnails="false"
          >
            <template #item="slotProps">
              <img
                :src="slotProps.item.itemImageSrc"
                :alt="slotProps.item.alt"
                style="width: 100%; display: block"
              />
            </template>
            <template #thumbnail="slotProps">
              <img
                :src="slotProps.item.thumbnailImageSrc"
                :alt="slotProps.item.alt"
                style="display: block"
              />
            </template>
          </Galleria>
        </div>

        <div class="product-wrap-top-right">
          <div class="product-name">
            LG - 65” Class UQ70 Series LED 4K UHD Smart webOS TV
          </div>

          <div class="product-sub">
            <span>Model: 8430288C2C</span>

            <span>ID: P9C3KC93CK</span>
          </div>

          <div class="product-rating">
            <span>{{ product.rating_count }}</span>

            <Rating
              :modelValue="product.rating_count"
              :stars="5"
              :readonly="true"
              :cancel="false"
              style="margin: 0 1rem"
            />
            <span>({{ product.review_count }})</span>
          </div>

          <div class="product-price">
            <div class="ada-label">₳</div>
            1.258
          </div>

          <div class="product-bottom">
            <div class="product-bottom-button">Buy now</div>

            <div class="product-bottom-bookmark" v-tooltip.top="'Add to cart'">
              <i class="pi pi-plus" />
            </div>

            <div class="product-bottom-bookmark" v-tooltip.top="'Save publication'">
              <i class="pi pi-bookmark" />
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import productAPI from "@/pages/product/api";
import MainHeader from "@/components/MainHeader.vue";
import { ref } from "vue";
import { Lucid, getAddressDetails } from "lucid-cardano";
import { balanceTx } from "@/api/wallet-api";

export default {
  components: {
    MainHeader,
  },
  setup() {
    const { lockingEndpoint } = productAPI();

    const images = ref([
      {
        itemImageSrc:
          "https://pisces.bbystatic.com/image2/BestBuy_US/images/products/6501/6501938_rd.jpg",
        thumbnailImageSrc:
          "https://pisces.bbystatic.com/image2/BestBuy_US/images/products/6501/6501938_rd.jpg",
        alt: "Description for Image 1",
        title: "Title 1",
      },
      {
        itemImageSrc:
          "https://pisces.bbystatic.com/image2/BestBuy_US/images/products/6553/6553101_sd.jpg",
        thumbnailImageSrc:
          "https://pisces.bbystatic.com/image2/BestBuy_US/images/products/6553/6553101_sd.jpg",
        alt: "Description for Image 2",
        title: "Title 2",
      },
      {
        itemImageSrc:
          "https://pisces.bbystatic.com/image3/BestBuy_US/images/products/6537/6537363_sd.jpg",
        thumbnailImageSrc:
          "https://pisces.bbystatic.com/image3/BestBuy_US/images/products/6537/6537363_sd.jpg",
        alt: "Description for Image 3",
        title: "Title 3",
      },
    ]);

    const responsiveOptions = ref([
      {
        breakpoint: "991px",
        numVisible: 4,
      },
      {
        breakpoint: "767px",
        numVisible: 3,
      },
      {
        breakpoint: "575px",
        numVisible: 1,
      },
    ]);

    const product = ref({
      rating_count: 4.8,
      review_count: 80,
    });

    return {
      images,
      product,
      lockingEndpoint,
      responsiveOptions,
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
.product {
  display: flex;
  justify-content: center;
  min-height: 100vh;
}

.product-price {
  text-align: left;
  margin-top: 1rem;
  font-size: var(--text-size-i);
  font-weight: 600;
  display: flex;
  align-items: baseline;
}

.ada-label {
  font-weight: 400;
  color: var(--text-b);
  font-size: var(--text-size-g);
  margin-right: 0.5rem;
}

.product-wrap {
  width: 80%;
}

.product-bottom {
  display: flex;
  width: 100%;
  align-items: center;
  margin-top: 2rem;
  justify-content: space-between;
}

.product-bottom-button {
  width: 80%;
  background: var(--blue-c);
  padding: 0.75rem;
  border-radius: 8px;
  color: var(--text-w);
  font-weight: 500;
  cursor: pointer;
}

.product-bottom-bookmark {
  width: 10%;
  cursor: pointer;
}

.product-bottom-bookmark i {
  font-size: var(--text-size-e);
}

.product-wrap-top-left {
  height: inherit;
  text-align: center;
  width: 65%;
  display: flex;
  justify-content: center;
  align-items: center;
}

.product-wrap-top-right {
  height: inherit;
  text-align: center;
  width: 35%;
}

.product-wrap-top {
  height: 800px;
  margin-top: 84px;
  display: flex;
  flex-wrap: wrap;
  justify-content: space-between;
}

.product-name {
  font-size: var(--text-size-g);
  font-weight: 700;
  margin-top: 4rem;
  text-align: left;
}

.product-sub {
  text-align: left;
  font-size: var(--text-size-b);
  margin-top: 1rem;
}

.product-sub span {
  margin-right: 1rem;
}

.product-rating {
  margin-top: 1rem;
  display: flex;
}

.product-rating {
  margin-right: 0.5rem;
  font-size: var(--text-size-c);
  color: var(--text-b);
  align-items: center;
}
</style>
