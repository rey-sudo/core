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

    <header class="p-header">
      <div class="p-header-wrap">
        <img
          class="p-header-wrap-logo"
          @click="handleRoute('store')"
          src="@/assets/logo-black.svg"
          alt=""
        />

        <div class="p-header-wrap-nav">
          <a @click="handleRoute('store')"> Todo </a>
          <a href="https://www.scatdao.com/team" target="_blank"> Ultimo </a>
          <a
            href="https://scatdao.gitbook.io/dao/platform/introduction"
            target="_blank"
            >Ofertas</a
          >
          <a href="https://linktr.ee/scatdao" target="_blank">Categoría</a>
          <a
            href="file:///home/debian/Documents/GitHub/Arka/z/latex/arka.pdf"
            target="_blank"
            >Tecnología</a
          >
        </div>
      </div>
    </header>

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
.p-header-wrap-logo {
  cursor: pointer;
}

.p-header {
  display: flex;
  justify-content: center;
  align-items: center;
  width: inherit;
}

.p-header .p-header-wrap {
  display: flex;
  width: inherit;
  align-items: center;
  justify-content: space-between;
  padding: 0.5rem 1rem;
  position: relative;
  background: var(--base-a);
  border-bottom: 1px solid var(--border-a);
}

.p-header .p-header-wrap .p-header-wrap-nav {
  display: flex;
  align-items: center;
  justify-content: space-between;
  white-space: nowrap;
  font-size: var(--text-size-b);
}

.p-header .p-header-wrap .p-header-wrap-nav a {
  color: var(--text-a);
  margin: 0 12px;
  font-size: var(--text-size-a);
  cursor: pointer;
}

h1 {
  font-size: var(--text-size-g);
  text-align: center;
  font-weight: 600;
  letter-spacing: -0.03em;
  background: inherit;
}

h2 {
  font-size: var(--text-size-d);
  font-weight: 600;
  background: inherit;
}

.mask {
  position: absolute;
  width: inherit;
  height: inherit;
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
  .p-header .p-header-wrap .p-header-wrap-nav {
    display: none;
  }
  .p-header .p-header-wrap {
    border: initial;
  }
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
