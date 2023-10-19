<template>
  <div class="p-grid">
   
    <div class="p-grid-item" v-for="(row, index) of getter__allProducts" :key="row">
      <div class="title">{{ row.title }}</div>

      <div class="p-grid-row">
        <div class="p-grid-row-slider prev" @click="slidePrev(index)">
          <i class="pi pi-angle-left" />
        </div>

        <div class="p-grid-row-slider next" @click="slideNext(index)">
          <i class="pi pi-angle-right" />
        </div>

        <div :class="customClass(index)">
          <div class="swiper-wrapper">
            <div
              class="swiper-slide card"
              v-for="item in row.items"
              :key="item"
            >
              <div class="card">
                <div class="card-header">
                  <div class="card-image">
                    <img src="@/pages/store/assets/150x150.webp" alt="" />
                  </div>
                </div>

                <div class="card-body">
                  <div class="card-body-name">
                    <span> {{ item.name }}</span>
                  </div>

                  <div class="card-body-diff">
                    <span> {{ formatPrice(item.price_diff) }} </span>
                  </div>

                  <div class="card-body-price">
                    <span class="dollar">$ </span>
                    <span> {{ formatPrice(item.price) }} </span>
                  </div>

                  <div class="card-body-upon">
                    {{ item.payment_type }}
                  </div>

                  <div
                    class="card-body-stock"
                    :class="{
                      blue: item.stock_supply,
                      red: !item.stock_supply,
                    }"
                  >
                    <span> {{ item.stock_supply }} Disponible</span>
                  </div>

                  <div
                    class="card-body-shipping"
                    :class="{
                      green: !item.shipping_tax,
                      gray: item.shipping_tax,
                    }"
                  >
                    <span v-if="!shipping_tax">
                      <span> {{ item.shipping_label }} </span>
                      <i class="pi pi-bolt" />
                    </span>
                  </div>
                </div>

                <div class="card-bottom">
                  <div
                    class="card-badge"
                    :style="{ color: item.discount_color }"
                  >
                    <span>{{ item.discount_label }}</span>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import Swiper from "swiper/bundle";
import "swiper/css/bundle";
import storeAPI from '@/pages/store/composable/store-api';
import { useRouter } from "vue-router";

export default {
  setup() {
    const router = useRouter();

    const { getter__allProducts } = storeAPI();

    return { router, getter__allProducts };
  },

  data() {
    return {
      sliderList: [],
    };
  },
  mounted() {
    for (let index = 0; index < this.getter__allProducts?.length; index++) {
      let prov = "." + "swiperInstance" + index;

      const slicer = new Swiper(prov, {
        effect: "slide",
        loop: false,
        slidesPerView: "auto",
        spaceBetween: 20,
        resistanceRatio: 0.5,
        centeredSlidesBounds: true,
        direction: "horizontal",
      });

      this.sliderList.push(slicer);
    }
  },
  methods: {
    customClass(index) {
      return `swiper swiperInstance${index}`;
    },
    slideNext(index) {
      this.sliderList[index].slideNext();
    },
    slidePrev(index) {
      this.sliderList[index].slidePrev();
    },
    formatPrice(num) {
      const price = num || 0;

      const formattedPrice = price.toLocaleString("en-US", {
        style: "currency",
        currency: "COP",
      });

      return formattedPrice;
    },
  },
};
</script>

<style lang="css" scoped>
.p-grid {
  display: grid;
  grid-template-columns: 1fr;
  grid-auto-rows: minmax(100px, auto);
  gap: 20px;
  padding: 0 4rem;
}

.dollar {
  font-size: var(--text-size-a);
  vertical-align: top;
}

.p-grid-item {
  display: flex;
  flex-direction: column;
}

.card-body-price {
  font-weight: 600;
  text-align: left;
  font-size: var(--text-size-c);
}

.card-body-diff {
  font-weight: 400;
  text-align: left;
  margin-top: 2rem;
  font-size: var(--text-size-a);
  text-decoration: line-through;
  color: var(--text-b);
}

.title {
  font-size: var(--text-size-c);
  font-weight: 600;
  text-align: start;
  line-height: 82px;
  color: var(--text--a);
}

.p-grid-row {
  display: flex;
  align-items: center;
}

.p-grid-row-slider {
  width: 60px;
  height: 60px;
  position: absolute;
  z-index: 100;
  border-radius: 50%;
  font-size: var(--text-size-d);
  align-items: center;
  display: flex;
  justify-content: center;
  cursor: pointer;
  background: transparent;
  color: transparent;
  transition: var(--button-transition-a);
}

.p-grid-row-slider:hover {
  background: var(--base-w-a);
  box-shadow: var(--shadow-c);
  color: var(--text-w-a);
}

.p-grid-row-slider.prev {
  left: 1rem;
}

.p-grid-row-slider.next {
  right: 1rem;
}

.swiper {
  padding: 1rem 0;
}

.swiper-wrapper {
  width: calc(100vw - 8rem);
  display: flex;
  align-items: center;
  position: initial;
}

::v-deep(.swiper-slide) {
  width: 300px !important;
}

.swiper-slide:hover {
  transform: translateY(-5px);
}

.card {
  width: 300px;
  height: 600px;
  background: var(--base-a);
  border-radius: 12px;
  transition: box-shadow 0.25s ease-in-out 0s, transform 0.25s ease 0s;
  display: flex;
  flex-direction: column;
  overflow: hidden;
  cursor: pointer;
}

.card-body-stock {
  margin-top: 0.5rem;
  text-align: left;
  font-size: var(--text-b);
  font-size: var(--text-size-a);
  font-weight: 600;
}

.card-body-shipping {
  margin-top: 0.5rem;
  text-align: left;
  font-size: var(--text-size-a);
  font-weight: 600;
}

.card:hover {
  box-shadow: var(--shadow-d);
}
.card-header {
  flex-basis: 45%;
  background-size: cover;
  background-repeat: no-repeat;
  background-position-y: 50%;
  padding: 1rem;
}

.card-image {
  height: 100%;
  display: flex;
  align-items: center;
  justify-content: center;
  border-radius: 12px;
}

.card-image img {
  width: 150px;
  height: 150px;
}

.card-body {
  flex-basis: 45%;
  padding: 0rem 1rem;
}

.card-body-name {
  color: var(--text-a);
  font-size: var(--text-size-b);
  text-transform: capitalize;
  text-align: left;
  font-weight: 500;
  display: flex;
  align-items: center;
  justify-content: center;
}

.card-body-upon {
  color: var(--text-b);
  font-size: var(--text-size-a);
  text-align: left;
  font-weight: initial;
  margin-top: 2rem;
  text-transform: capitalize;
}

.card-bottom {
  flex-basis: auto;
}
.card-badge {
  font-size: var(--text-size-a);
  height: 30px;
  position: relative;
  color: var(--text-a);
  font-weight: 600;
  margin-top: auto;
}

.card-badge span {
  z-index: 100;
  position: relative;
  text-transform: capitalize;
  height: 30px;
  display: flex;
  text-align: center;
  align-items: center;
  justify-content: center;
}

.card-badge::before,
.card-badge::after {
  content: "";
  position: absolute;
  top: 0;
  width: 50%;
  height: 30px;
  background: var(--base-b);
}

.card-badge::before {
  left: 0;
  transform: skewX(-45deg);
  transform-origin: bottom left;
  border-top-left-radius: 4px;
  border-top-right-radius: 0px;
  border-bottom-right-radius: 0px;
  border-bottom-left-radius: 0px;
  border-right: transparent;
}

.card-badge::after {
  right: 0;
  transform: skewX(45deg);
  transform-origin: bottom right;
  border-top-right-radius: 4px;
  border-top-left-radius: 0px;
  border-bottom-left-radius: 0px;
  border-bottom-right-radius: 0px;
  border-right: transparent;
}

@media (max-width: 768px) {
  .p-grid {
    grid-template-columns: 1fr;
  }
}
</style>
