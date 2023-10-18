<template>
  <div class="p-slider">
    <div class="p-slider-grid">
      <div class="p-slider-grid-item left">


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
      autoplay: true,
      freeMode: true,
      loop: true
    });
  }
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

@media screen and (max-width: 767px) {
  .p-slider{
    display: initial;
  }
}
</style>
