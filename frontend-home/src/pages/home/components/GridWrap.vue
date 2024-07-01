<template>
  <div class="grid">
    <div class="grid-item" v-for="row of getter__allProducts" :key="row">
      <div class="grid-item-title">{{ row.title }}</div>

      <div class="grid-row">
        <div
          class="card"
          v-for="item in row.items"
          :key="item"
          @click="handleClick(item.id)"
        >
          <div class="card-header">
            <img class="card-image" :src="item.image" alt="" />
          </div>

          <div class="card-body">
            <div class="card-body-name">
              <span> {{ item.name.slice(0, 80) }}...</span>
            </div>

   

            <div class="card-body-rating">
              <Rating
                :modelValue="item.rating_count"
                :stars="5"
                :readonly="true"
                :cancel="false"
              />
              <span>({{ item.review_count }})</span>
            </div>
            <div class="card-body-price">
              <span> {{ formatPrice(item.price) }}</span>
            </div>

            
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import homeAPI from "@/pages/home/composable/home-api";
import { useRouter } from "vue-router";

export default {
  setup() {
    const router = useRouter();

    const { getter__allProducts } = homeAPI();

    return { router, getter__allProducts };
  },
  methods: {
    handleClick(id) {
      this.router.push({ name: "product", params: { id } });
    },

    formatPrice(num) {
      const price = num || 0;

      const customCurrencySymbol = "₳";

      const formattedNumber = `  ${price.toLocaleString(
        "en-US"
      )} ${customCurrencySymbol}`;

      return formattedNumber;
    },
  },
};
</script>

<style lang="css" scoped>
::v-deep(.p-rating-icon) {
  width: var(--text-size-a);
  height: var(--text-size-a);
  color: var(--yellow-b);
}

.card-body-rating {
  display: flex;
  align-items: center;
  margin-top: 0.5rem;
}

.card-body-rating span {
  font-size: var(--text-size-a);
  margin-left: 0.5rem;
  color: var(--blue-a);
}

.card-body-collateral {
  text-align: left;
  font-size: var(--text-size-b);
  display: flex;
  align-items: center;
  font-weight: 400;
  color: var(--text-a);
  margin-top: 0.25rem;
}

.card-body-price {
  font-weight: 500;
  text-align: left;
  font-size: var(--text-size-e);
  color: var(--text-a);
  margin-top: 0.5rem;
}

.grid-item-title {
  font-size: var(--text-size-h);
  font-weight: bold;
  text-align: left;
  color: var(--text-a);
  margin: 2rem 0;
  font-weight: 700;
}

.grid {
  display: grid;
  grid-template-columns: 1fr;
  grid-auto-rows: minmax(100px, auto);
  gap: 1rem;
  padding: 0 4rem;
  min-height: 100vh;
  border-top-left-radius: 0px;
  border-top-right-radius: 0px;
  background: var(--base-b);
}

.grid-item {
  display: flex;
  flex-direction: column;
}

.grid-row {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
  gap: 1rem;
}

.card {
  width: 100%;
  height: 600px;
  transition: box-shadow 0.25s ease-in-out 0s, transform 0.25s ease 0s;
  display: flex;
  flex-direction: column;
  overflow: hidden;
  cursor: pointer;
  margin: auto;
  background: var(--base-a);
    padding: 1rem;
}

.card-header {
  flex-basis: 50%;
  overflow: hidden;
  display: flex;
  align-items: center;
  justify-content: center;
}

.card-image {
}

.card-image {
  width: 200px;
  height: 200px;
  display: flex;
  justify-content: center;
  border-radius: 12px;
  overflow: hidden;
  object-fit: contain;
}

.card-body {
  flex-basis: 25%;
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: flex-start;
}

.card-body-name {
  color: var(--text-a);
  font-size: var(--text-size-c);
  display: flex;
  align-items: center;
  justify-content: center;
  font-weight: 400;
  text-transform: capitalize;
}

.card-body-name:hover {
  text-decoration: underline;
}

.card-body-name span {
  text-align: left;
  line-height: 24px;
}

.card-body-name span::first-letter {
  text-transform: capitalize;
}

.card-body-slot {
  color: var(--text-b);
  font-size: var(--text-size-a);
  text-align: left;
  font-weight: initial;
  margin-bottom: 0.5rem;
  text-transform: capitalize;
}

.card-bottom {
  flex-basis: 5%;
}

.badge {
  margin-left: 0.5rem;
  margin-top: 0.5rem;
}

.card-badge {
  font-size: var(--text-size-a);
  height: 30px;
  position: relative;
  color: var(--text-b);
  font-weight: 400;
  margin-top: auto;
  display: flex;
  align-items: center;
  justify-content: center;
}

.card-badge span {
  position: relative;
  text-transform: capitalize;
  height: 30px;
  display: flex;
  text-align: center;
  align-items: center;
  justify-content: center;
  z-index: -1;
}

.card-badge::before,
.card-badge::after {
  content: "";
  position: absolute;
  top: 0;
  width: 50%;
  height: 30px;
  transition: box-shadow 0.1s ease-in-out 0s, transform 0.25s ease 0s;
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
  background: var(--base-b);
  z-index: -1;
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
  z-index: -2;
  background: transparent;
  background: var(--base-b);
}

@media only screen and (max-width: 600px) {
  .grid {
    display: none;
  }
}
</style>
@/pages/home/composable/store-api
