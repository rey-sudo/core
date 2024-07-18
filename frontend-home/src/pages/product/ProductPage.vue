<template>
  <div class="product" v-if="getProductData">
    <MainHeader />
    <MobileHeader />
    <div class="product-body" >
      <div class="product-body-wrap">
        <HeadView />
        <MobileHeadView />
      </div>

      <div class="product-body-body"></div>
    </div>
  </div>
</template>

<script>
import productAPI from "@/pages/product/api";
import MainHeader from "@/components/header/MainHeader.vue";
import MobileHeader from "@/components/header/MobileHeader.vue";
import HeadView from "@/pages/product/components/HeadView.vue";
import MobileHeadView from "@/pages/product/components/MobileHeadView.vue";

export default {
  components: {
    MainHeader,
    MobileHeader,
    HeadView,
    MobileHeadView,
  },
  setup() {
    const { getProduct, getProductData } = productAPI();

    const setupData = (id) => {
      getProduct({ id: id });
    };

    return {
      setupData,
      getProductData,
    };
  },
  data() {
    return {};
  },

  created() {
    this.$watch(
      () => this.$route.params,
      (params) => this.setupData(params.id),
      { immediate: true }
    )();
  },
  methods: {
    scrollTop() {
      window.scrollTo({
        top: 0,
        behavior: "auto",
      });
    },
  },

  mounted() {
    this.scrollTop();
  },
};
</script>

<style lang="css" scoped>
.product-body {
  padding: 0 11%;
  width: 100%;
}

.product {
  display: flex;
  justify-content: center;
  min-height: 100vh;
  width: 100%;
  flex-direction: column;
  align-items: center;
}

.product-body-wrap {
  min-height: 150vh;
  margin-top: calc(40px + 2rem);
  display: flex;
  flex-wrap: wrap;
  flex-direction: column;
}

@media only screen and (max-width: 767px) {
  .product-body {
    width: 100%;
  }

  .product-body-wrap {
    margin-top: 100px;
  }
}

/* Tablets and small deskbanners */
@media only screen and (min-width: 768px) and (max-width: 991px) {
  .product-body {
    width: 100%;
  }

  .product-body-wrap {
    margin-top: 100px;
  }
}

/* Medium deskbanners */
@media only screen and (min-width: 992px) and (max-width: 1199px) {
  .product-body {
    width: 100%;
  }

  .product-body-wrap {
    margin-top: 100px;
  }
}

/* Large deskbanners and widescreen monitors */
@media only screen and (min-width: 1200px) {
  /* CSS rules for large deskbanners and widescreen monitors */
}
</style>
