<template>
  <div class="p-slider">
    <div class="p-slider-grid">
      <div class="p-slider-grid-item left">
        <div class="navigation-button prev" @click="slidePrev(index)">
          <i class="pi pi-angle-up" />
        </div>

        <div class="navigation-button next" @click="slideNext(index)">
          <i class="pi pi-angle-down" />
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
              v-for="item in getter__productData.theme.slider_images"
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

    </div>
  </div>
</template>

<script>
import Swiper from "swiper/bundle";
import "swiper/css/bundle";
import landAPI from "@/pages/land/composable/land-api";

export default {

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
      direction: "vertical",
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
  width: 100%;
  height: 300px;
  min-height: 300px;
  margin-top: 2rem;
  background: var(--base-a);
  border-radius: 12px;
  overflow: hidden;
  display: none;
}

.p-slider-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(0, 1fr));
  gap: 0px;
  justify-content: center;
  padding: 0;
  height: inherit;
  box-sizing: border-box;
}

.p-slider-grid-item {
  text-align: center;
  height: inherit;
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
  width: 300px;
  height: 300px;
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
  bottom: -2rem;
}

.navigation-button.next {
  left: 8rem;
  bottom: -2rem;
}


@media screen and (max-width: 767px) {
  .p-slider{
    display: initial;
  }
}
</style>
