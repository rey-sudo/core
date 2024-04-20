<template>
  <div class="store">
    <div class="top">
      <MainHeader />
      <MobileHeader />
      <div class="top-mask" />

      <div class="top-banner"></div>
    </div>

    <GridWrap />
    <MobileGrid />
  </div>
</template>

<script>
import MainHeader from "@/components/MainHeader.vue";
import MobileHeader from "@/components/MobileHeader.vue";
import GridWrap from "@/pages/home/components/GridWrap.vue";
import MobileGrid from "@/pages/home/components/MobileGrid.vue";
import storeAPI from "@/pages/home/composable/home-api";
import { useRouter } from "vue-router";

export default {
  components: {
    MainHeader,
    MobileHeader,
    MobileGrid,
    GridWrap,
  },

  setup() {
    const router = useRouter();

    const { action__getAllProducts } = storeAPI();

    action__getAllProducts()
      .then(() => {})
      .catch((err) => console.error(err));

    return { router, action__getAllProducts };
  },
  data() {
    return {
      selectedTab: "all",
      
    };
  },
};
</script>

<style lang="css" scoped>
.store {
  width: 100%;
  min-height: 100vh;
  display: flex;
  flex-direction: column;
  background: var(--blue-b);
}

.store .top {
  width: inherit;
  height: 600px;
  min-height: 600px;
  z-index: 1;
  position: relative;
  inset: 0px 0px -1px;
  background-size: contain;
  display: flex;
  flex-direction: column;
}

.store .top .top-mask {
  width: inherit;
  height: inherit;
  z-index: 2;
  position: absolute;
  backdrop-filter: blur(0px);
  background: linear-gradient(180deg, transparent 90%, rgba(0, 0, 0, 0.1) 100%);
}

.store .top .top-banner {
  display: grid;
  grid-template-columns: 1fr 1fr;
  grid-gap: 2rem;
  z-index: 100;
  padding: 0 4rem;
}

@media only screen and (max-width: 600px) {

  .store .top {
    height: 300px;
    min-height: 300px;
  }
}
</style>
@/pages/home/composable/store-api
