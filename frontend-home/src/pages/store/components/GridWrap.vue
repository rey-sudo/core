<template>
  <div class="grid">
    <div class="grid-item" v-for="row of getter__allProducts" :key="row">
      <div class="grid-item-title">{{ row.title }}</div>

      <div class="grid-row">
        <!---->
        <div
          class="card"
          v-for="item in row.items"
          :key="item"
          @click="handleClick(item.pid)"
        >
          <div class="card-header">
            <div class="card-image">
              <img :src="item.image" alt="" />
            </div>
          </div>

          <div class="card-body">
            <div class="card-body-name">
              <span> {{ item.name }}</span>
            </div>
            <div class="card-body-price">
              <span> {{ formatPrice(item.price) }}</span>
            </div>
            <div
              class="card-body-stock"
              :class="{
                white: item.stock_supply,
                red: !item.stock_supply,
              }"
            >
              <span> {{ item.stock_supply }} Stock</span>
            </div>

            <div class="card-body-collateral">
              <span> {{ item.collateral }} ADA Collateral</span>
            </div>
          </div>

          <div class="card-bottom">
            <div class="card-badge" :style="{ color: item.discount_color }">
              <span>{{ item.discount_label }}</span>
              <span>
                <svg
                  viewBox="0 0 22 22"
                  aria-label="badge"
                  class="badge"
                  width="1rem"
                  height="1rem"
                  role="img"
                  fill="#0069f5"
                >
                  <g>
                    <path
                      d="M20.396 11c-.018-.646-.215-1.275-.57-1.816-.354-.54-.852-.972-1.438-1.246.223-.607.27-1.264.14-1.897-.131-.634-.437-1.218-.882-1.687-.47-.445-1.053-.75-1.687-.882-.633-.13-1.29-.083-1.897.14-.273-.587-.704-1.086-1.245-1.44S11.647 1.62 11 1.604c-.646.017-1.273.213-1.813.568s-.969.854-1.24 1.44c-.608-.223-1.267-.272-1.902-.14-.635.13-1.22.436-1.69.882-.445.47-.749 1.055-.878 1.688-.13.633-.08 1.29.144 1.896-.587.274-1.087.705-1.443 1.245-.356.54-.555 1.17-.574 1.817.02.647.218 1.276.574 1.817.356.54.856.972 1.443 1.245-.224.606-.274 1.263-.144 1.896.13.634.433 1.218.877 1.688.47.443 1.054.747 1.687.878.633.132 1.29.084 1.897-.136.274.586.705 1.084 1.246 1.439.54.354 1.17.551 1.816.569.647-.016 1.276-.213 1.817-.567s.972-.854 1.245-1.44c.604.239 1.266.296 1.903.164.636-.132 1.22-.447 1.68-.907.46-.46.776-1.044.908-1.681s.075-1.299-.165-1.903c.586-.274 1.084-.705 1.439-1.246.354-.54.551-1.17.569-1.816zM9.662 14.85l-3.429-3.428 1.293-1.302 2.072 2.072 4.4-4.794 1.347 1.246z"
                    ></path>
                  </g>
                </svg>
              </span>
            </div>
          </div>
        </div>
        <!---->
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

      const customCurrencySymbol = "ADA";

      const formattedNumber = `${price.toLocaleString(
        "en-US"
      )} ${customCurrencySymbol}`;

      return formattedNumber;
    },
  },
};
</script>

<style lang="css" scoped>
.grid {
  display: grid;
  grid-template-columns: 1fr;
  grid-auto-rows: minmax(100px, auto);
  gap: 20px;
  padding: 0 2rem;
  min-height: 100vh;
  border-top-left-radius: 16px;
  border-top-right-radius: 16px;
  background: var(--base-a);
}

.grid-item {
  display: flex;
  flex-direction: column;
}

.card-body-price {
  font-weight: 600;
  text-align: left;
  font-size: var(--text-size-b);
  margin-bottom: 0.5rem;
  color: var(--blue-a);
}

.grid-item-title {
  font-size: var(--text-size-f);
  font-weight: 700;
  text-align: start;
  line-height: 125px;
  color: var(--text-a);
  padding-left: 1.25rem;
  letter-spacing: -0.02em;
}

.grid-row {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
  gap: 1rem;
}

.card {
  width: calc(300px - 2rem);
  height: 600px;
  border-radius: calc(1rem + 16px);
  transition: box-shadow 0.25s ease-in-out 0s, transform 0.25s ease 0s;
  display: flex;
  flex-direction: column;
  overflow: hidden;
  cursor: pointer;
  padding: 1.25rem;
  padding-bottom: 0;
  margin: auto;
  line-height: 1.5rem;
  border: 1px solid transparent;
}

.card:hover {
  transform: translateY(-0.25rem);
  box-shadow: var(--shadow-a);
  border: 1px solid var(--border-a);
}

.card-body-stock {
  margin-bottom: 0.5rem;
  text-align: left;
  color: var(--text-b);
  font-size: var(--text-size-b);
}

.card-body-collateral {
  text-align: left;
  font-size: var(--text-size-b);
  display: flex;
  align-items: center;
  color: var(--text-b);
}

.card-header {
  flex-basis: 50%;
  overflow: hidden;
}

.card-image {
  height: 100%;
  display: flex;
  justify-content: center;
}

.card-image img {
  width: calc(300px - 2rem);
  height: calc(300px - 2rem);
  min-width: calc(300px - 2rem);
  max-width: calc(300px - 2rem);
  border-radius: 16px;
}

.card-body {
  flex-basis: 45%;
}

.card-body-name {
  color: var(--text-a);
  font-size: var(--text-size-b);
  text-align: left;
  display: flex;
  align-items: center;
  justify-content: center;
  margin-bottom: 1rem;
  font-weight: 400;
  text-transform: lowercase;
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
}

.card-badge::before,
.card-badge::after {
  content: "";
  position: absolute;
  top: 0;
  width: 50%;
  height: 30px;
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
  background: transparent;
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
  z-index: 1;
  background: transparent;
}

@media only screen and (max-width: 600px) {
  .grid {
    display: none;
  }
}
</style>
