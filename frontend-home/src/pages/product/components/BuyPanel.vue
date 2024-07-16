<template>
  <div class="head-info">
    <span>5 available</span>
  </div>

  <div class="head-name">
    Apple 2023 MacBook Pro Laptop M3 Pro chip with 11‑core CPU, 14‑core GPU:
    14.2-inch Liquid Retina XDR Display, 18GB Unified Memory, 512GB SSD Storage.
    Works with iPhone/iPad; Space Black
  </div>

  <div class="head-legend">
    <span>Model: PLACCOG7OQVQY2BX </span>
    <span>SKU: P9C3KC93CK</span>
  </div>

  <div class="head-rating">
    <Rating
      :modelValue="product.rating_count"
      :stars="5"
      :readonly="true"
      :cancel="false"
      style="margin-right: 0.5rem"
    />
    <span>{{ product.rating_count }}</span>

    <span>({{ product.review_count }} Reviews)</span>
  </div>

  <div class="head-price">
    <div class="ada-label">₳</div>
    <span>3,024</span>
  </div>

  <InfoIcons />

  <div class="head-button buyButton">
    <div @click="buyProduct">Buy now</div>
  </div>
</template>

<script>
import InfoIcons from "./InfoIcons.vue";
import { ref } from "vue";
export default {
  components: {
    InfoIcons,
  },

  setup() {
    const product = ref({
      rating_count: 4.8,
      review_count: 558,
    });
    return { product };
  },
};
</script>

<style lang="css" scoped>
.head-info {
  font-size: var(--text-size-b);
  font-weight: 400;
  text-align: left;
  color: var(--primary-c);
}

.head-name {
  font-size: var(--text-size-g);
  font-weight: 500;
  text-align: left;
  width: 100%;
  margin-top: 1rem;
  line-height: 2.25rem;
}

.head-legend {
  text-align: left;
  font-size: var(--text-size-a);
  margin-top: 1rem;
  font-weight: 400;
  color: var(--text-a);
}

.head-legend span {
  margin-right: 1rem;
}

.head-rating {
  font-size: var(--text-size-c);
  align-items: center;
  display: flex;
  padding-top: 1rem;
  padding-bottom: 1rem;
  width: 90%;
}

.head-rating span {
  margin-right: 0.5rem;
  font-size: var(--text-size-b);
  color: var(--primary-c);
  font-weight: 600;
}

.head-rating span:nth-child(3) {
  font-weight: 400;
  font-size: var(--text-size-a);
  color: var(--primary-c);
}

.head-price {
  text-align: left;
  margin-top: initial;
  font-size: var(--text-size-h);
  font-weight: 600;
  display: flex;
  align-items: baseline;
  margin-top: 1rem;
}

.head-price .ada-label {
  font-weight: 400;
  color: var(--text-a);
  font-size: var(--text-size-g);
  margin-right: 0.5rem;
  margin-bottom: 2px;
}

.head-button {
  border: 1px solid var(--primary-b);
  width: 90%;
  border-radius: 4px;
  color: var(--text-a);
  font-weight: 600;
  cursor: pointer;
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 0.75rem;
  text-align: center;
  margin-top: 1rem;
  transition: var(--transition-a);
  background: var(--primary-b);
}

.head-button.buyButton {
  margin-top: 2rem;
}

.head-button:hover {
  opacity: 0.8;
}
</style>
