<template>
  <div
    class="p-page1"
    id="page1"
    :style="{
      background: getter__productData.theme.config.page_1.background_color,
    }"
  >
    <div class="mask">
      <img
        :src="
          getter__productData.space_url +
          getter__productData.theme.config.page_1.mask
        "
      />
    </div>

    <div class="p-page1-wrap">
      <div class="p-page1-wrap-title">
        <h1>
          {{ getter__productData.theme.title }}
        </h1>
      </div>
      <div class="p-page1-wrap-subtitle">
        <h2>
          {{ getter__productData.theme.subtitle }}
        </h2>
      </div>

      <ImageSwiper />
      <ImageMobile />
    </div>
  </div>
</template>

<script>
import ImageSwiper from "@/pages/land/components/ImageSwiper.vue";
import ImageMobile from "@/pages/land/components/mobile/ImageMobile.vue";
import landAPI from "@/pages/land/composable/land-api";
import { useRouter } from "vue-router";

export default {
  components: {
    ImageSwiper,
    ImageMobile,
  },
  setup() {
    const router = useRouter();

    const { getter__productData } = landAPI();

    return {
      router,
      getter__productData,
    };
  },
  methods: {
    handleReload() {
      location.reload();
    },

    handleRoute(name) {
      this.router.push({ name: name });
    },
  },
};
</script>

<style lang="css" scoped>


h1 {
  font-size: var(--text-size-g);
  text-align: center;
  font-weight: 600;
  letter-spacing: -0.03em;
  background: inherit;
  margin: 1rem 0;
}

h2 {
  font-size: var(--text-size-d);
  font-weight: 500;
  letter-spacing: -0.01em;
  background: inherit;
}

.mask {
  position: absolute;
  width: inherit;
  height: inherit;
  min-height: inherit;
}

.mask img {
  width: inherit;
  height: inherit;
  object-fit: cover;
  object-position: bottom;
}

.p-page1 {
  display: flex;
  flex-direction: column;
  align-items: center;
  width: 100%;
  height: 100%;
  min-height: 100vh;
  background-size: contain;
  background-repeat: repeat;
}

.p-page1 .p-page1-wrap {
  margin: auto 0;
  position: relative;
  display: flex;
  flex-direction: column;
  align-items: center;
  animation-name: bodyAnimation;
  animation-duration: 4s;
  animation-timing-function: ease-in-out;
  animation-fill-mode: forwards;
  width: 1300px;
  height: 800px;
  min-height: 800px;
  max-height: 800px;
  box-sizing: border-box;
  border-bottom: none;
}

@keyframes bodyAnimation {
  0% {
    opacity: 0;
    transform: translateY(-10px);
  }

  10% {
    opacity: 1;
    transform: translateY(0px);
  }
}

.p-page1 .p-page1-wrap .p-page1-wrap-title {
  max-width: 100%;
  padding: 0 1rem;
}

.p-page1 .p-page1-wrap .p-page1-wrap-subtitle {
  line-height: 0;
}

@media screen and (max-width: 767px) {

  .p-page1 .p-page1-wrap {
    width: 100%;
    height: 100%;
    min-height: 100%;
    max-height: 100%;
  }

  .p-page1 .p-page1-wrap .p-page1-wrap-subtitle {
    line-height: initial;
    padding: 0 1rem;
  }

  h1 {
    font-size: var(--text-size-e);
    color: var(--text-a);
  }

  h2 {
    font-size: var(--text-size-c);
    font-weight: 400;
    color: var(--text-a);
  }
}
</style>
