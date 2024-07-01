<template>
  <div>
    <div class="banner">
      <div class="banner-mask" />
      <Carousel
        :value="products"
        :numVisible="1"
        :numScroll="1"
        :page="0"
        :responsiveOptions="responsiveOptions"
        :showNavigators="false"
        :showIndicators="false"
        verticalViewPortHeight="300px"
        :autoplayInterval="5000"
        circular
      >
        <template #item="slotProps">
          <img class="image" :src="slotProps.data.image" />
        </template>
      </Carousel>
    </div>
  </div>
</template>

<script>
import { ref } from "vue";

export default {
  setup() {
    const products = ref([
      {
        name: "1",
        image:
          "https://www.asus.com/WebsitesBanner/CO/banners/rr7kwlphfplxipo6/rr7kwlphfplxipo6-0_0_desktop_0_1X.jpg",
      },

      {
        name: "2",
        image:
          "https://www.asus.com/WebsitesBanner/CO/banners/pw8rrokxjygdtfuz/pw8rrokxjygdtfuz-0_0_desktop_0_1X.jpg",
      },

      {
        name: "3",
        image:
          "https://www.asus.com/WebsitesBanner/CO/banners/pw8rrokxjygdtfuz/pw8rrokxjygdtfuz-0_0_desktop_0_1X.jpg",
      },

      {
        name: "4",
        image:
          "https://www.asus.com/WebsitesBanner/CO/banners/rr7kwlphfplxipo6/rr7kwlphfplxipo6-0_0_desktop_0_1X.jpg",
      },
    ]);
    const responsiveOptions = ref([
      {
        breakpoint: "1400px",
        numVisible: 2,
        numScroll: 1,
      },
      {
        breakpoint: "1199px",
        numVisible: 3,
        numScroll: 1,
      },
      {
        breakpoint: "767px",
        numVisible: 2,
        numScroll: 1,
      },
      {
        breakpoint: "575px",
        numVisible: 1,
        numScroll: 1,
      },
    ]);

    return { products, responsiveOptions };
  },
};
</script>

<style lang="css" scoped>
.image {
  height: 400px;
  object-fit: cover;
  width: 100%;
  position: relative;
}

.banner {
  width: inherit;
  height: 400px;
  min-height: 400px;
  z-index: 1;
  position: relative;
  inset: 0px 0px -1px;
  background-size: contain;
  display: flex;
  flex-direction: column;
  margin-top: 41px;
}

.banner-mask {
  width: inherit;
  height: inherit;
  height: 100px;
  width: 100%;
  bottom: 0;
  z-index: 3;
  position: absolute;
  backdrop-filter: blur(0px);
  background: linear-gradient(180deg, transparent 10%, var(--blue-a) 100%);
}

.banner-screen {
  display: grid;
  grid-template-columns: 1fr 1fr;
  grid-gap: 2rem;
  z-index: 100;
  padding: 0 4rem;
}

@media only screen and (max-width: 767px) {
  .store {
    background: var(--base-b);
  }

  .banner {
    height: 400px;
    min-height: 400px;
  }

  .banner-mask {
    display: none;
  }
}
</style>
