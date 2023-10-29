<template>
  <div class="p-slider">
    <div class="p-slider-grid">
      <div class="p-slider-grid-item left">
        <div class="navigation-button prev" @click="slidePrev(index)">
          <i class="pi pi-angle-left" />
        </div>

        <div class="navigation-button next" @click="slideNext(index)">
          <i class="pi pi-angle-right" />
        </div>

        <!--SLIDER-->
        <div
          style="
            --swiper-navigation-color: #fff;
            --swiper-pagination-color: #fff;
          "
          class="swiper itemSliderSwiper"
        >
          <div class="swiper-wrapper">
            <div
              v-for="item in getter__productData.theme.config.page_1.images"
              :key="item"
              class="swiper-slide swiper-no-mousewheel"
            >
              <div class="swiper-zoom-container">
                <img :src="getter__productData.space_url + item.url" />
              </div>
            </div>
          </div>
        </div>
        <!--SLIDER-->
      </div>
      <div class="p-slider-grid-item right">
        <PaymentWrap />
      </div>
    </div>
  </div>
</template>

<script>
import Swiper from "swiper/bundle";
import "swiper/css/bundle";

import landAPI from "@/pages/land/composable/land-api";
import PaymentWrap from "@/pages/land/components/PaymentWrap";

export default {
  components: {
    PaymentWrap,
  },
  setup() {
    const { getter__productData } = landAPI();

    return {
      getter__productData,
    };
  },
  data() {
    return {
      slider: null,
    };
  },
  mounted() {
    this.slider = new Swiper(".itemSliderSwiper", {
      zoom: true,
      mousewheel: false,
      autoplay: true,
      loop: true,
      direction: "horizontal",
    });
  },
  methods: {
    slidePrev() {
      this.slider.slidePrev();
    },

    slideNext() {
      this.slider.slideNext();
    },
  },
};
</script>

<style lang="css" scoped>
.p-slider {
  width: calc(1200px + 1rem);
  height: 600px;
  min-height: 600px;
  margin-top: 3rem;
  background: var(--base-a);
  border-radius: 34px;
  overflow: hidden;
}

.p-slider-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(0, 1fr));
  gap: 1rem;
  justify-content: center;
  padding: 0;
  height: inherit;
  box-sizing: border-box;
}

.p-slider-grid-item {
  text-align: center;
  height: inherit;
}

.p-slider-grid-item.left {
}

.p-slider-grid-item.right {
  display: flex;
  align-items: center;
  justify-content: center;
}

.swiper {
  width: 100%;
  height: 100%;
}

.swiper-slide {
  overflow: hidden;
  cursor: zoom-in;
}

.swiper-slide img {
  width: 600px;
  height: 600px;
}

::v-deep(.itemSliderSwiper .swiper-pagination-bullet) {
  width: 0.25rem;
  height: 0.75rem;
  border-radius: 0px;
}

.navigation-button {
  width: 50px;
  height: 50px;
  position: absolute;
  z-index: 100;
  border-radius: 6px;
  font-size: var(--text-size-d);
  align-items: center;
  display: flex;
  justify-content: center;
  cursor: pointer;
  background: rgba(255, 255, 255, 0.5);
  color: var(--text-a);
  transition: var(--button-transition-a);
}

.navigation-button:hover {
  background: var(--base-a);
  box-shadow: var(--shadow-c);
  color: var(--text-a);
}

.navigation-button.prev {
  left: 4rem;
  bottom: 2rem;
}

.navigation-button.next {
  left: 8rem;
  bottom: 2rem;
}

@media screen and (max-width: 767px) {
  .p-slider {
    display: none;
  }
}
</style>
