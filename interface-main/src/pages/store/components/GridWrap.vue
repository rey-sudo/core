<template>
  <div class="p-grid">
    <div class="p-grid-item" v-for="row of getter__allProducts" :key="row">
      <div class="title">{{ row.title }}</div>

      <div class="p-grid-row">
        <div
          class="card"
          v-for="item in row.items"
          :key="item"
          @click="handleClick(item.pid)"
        >
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
                gray: item.stock_supply,
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
                <span> {{ item.shipping_label }}</span>
                <i :class="item.shipping_icon" />
              </span>
            </div>
          </div>

          <div class="card-bottom">
            <div class="card-badge" :style="{ color: item.discount_color }">
              <span>{{ item.discount }}{{ item.discount_label }}</span>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import storeAPI from "@/pages/store/composable/store-api";
import { useRouter } from "vue-router";

export default {
  setup() {
    const router = useRouter();

    const { getter__allProducts } = storeAPI();

    return { router, getter__allProducts };
  },
  methods: {
    handleClick(pid) {
      this.router.push({ name: "land", params: { pid: pid } });
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
  padding: 0 3rem;
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
  margin-top: 1rem;
  font-size: var(--text-size-a);
  text-decoration: line-through;
  color: var(--text-b);
}

.title {
  font-size: var(--text-size-e);
  font-weight: 700;
  text-align: start;
  line-height: 82px;
  color: var(--text--a);
  padding-left: 1rem;
}

.p-grid-row {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
  gap: 1rem;
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

.card {
  width: calc(300px - 2rem);
  height: 600px;
  border-radius: 12px;
  transition: box-shadow 0.25s ease-in-out 0s, transform 0.25s ease 0s;
  display: flex;
  flex-direction: column;
  overflow: hidden;
  cursor: pointer;
  padding: 0 1rem;
}

.card:hover {
  box-shadow: var(--shadow-a);
  transform: translateY(-1rem);
}

.card-body-stock {
  margin-top: 0.5rem;
  text-align: left;
  font-size: var(--text-b);
  font-size: var(--text-size-a);
}

.card-body-shipping {
  margin-top: 0.5rem;
  text-align: left;
  font-size: var(--text-size-a);
  font-weight: 600;
  display: flex;
  align-items: center;
}

.card-body-shipping i {
  margin-left: 0.25rem;
}
.card-header {
  flex-basis: 50%;
  overflow: hidden;
}

.card-image {
  height: 100%;
  display: flex;
  align-items: center;
  justify-content: center;
}

.card-image img {
  width: calc(300px - 2rem);
  height: calc(300px - 2rem);
  min-width: calc(300px - 2rem);
  max-width: calc(300px - 2rem);
  border-radius: 12px;
}

.card-body {
  flex-basis: 45%;
}

.card-body-name {
  color: var(--text-a);
  font-size: var(--text-size-a);
  text-transform: capitalize;
  text-align: left;
  display: flex;
  align-items: center;
  justify-content: center;
}

.card-body-upon {
  color: var(--text-b);
  font-size: var(--text-size-a);
  text-align: left;
  font-weight: initial;
  margin-top: 1rem;
  text-transform: capitalize;
}

.card-bottom {
  flex-basis: 5%;
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
