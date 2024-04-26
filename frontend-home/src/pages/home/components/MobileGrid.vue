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
              {{ item.name }}
            </div>
            <div class="card-body-price">
              <span> {{ formatPrice(item.price) }}</span>
            </div>


            <div class="card-body-collateral">
              <span> {{ item.collateral }} ADA Collateral</span>
            </div>
          </div>
        </div>
        <!---->
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
  padding: 0;
  min-height: 100vh;
  border-top-left-radius: 16px;
  border-top-right-radius: 16px;
  background: var(--base-a);
  display: none;
}

.grid-item {
  display: flex;
  flex-direction: column;
}

.card-body-price {
  font-weight: 700;
  text-align: left;
  font-size: var(--text-size-b);
  color: var(--text-a);
  margin-top: 0.5rem;
}

.grid-item-title {
  font-size: var(--text-size-c);
  font-weight: 700;
  text-align: start;
  line-height: 75px;
  color: var(--text-a);
  padding-left: 0.75rem;
  letter-spacing: -0.02em;
}

.grid-row {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(150px, 1fr));
  gap: 0rem;
}

.card {
  width: calc(100% - 1.75rem);
  height: 300px;
  transition: box-shadow 0.25s ease-in-out 0s, transform 0.25s ease 0s;
  display: flex;
  flex-direction: column;
  overflow: hidden;
  cursor: pointer;
  padding: 0;
  padding-bottom: 0;
  margin: auto;
  line-height: 1.5rem;
  border: 1px solid transparent;
}
.card-body-stock {
  text-align: left;
  color: var(--text-b);
  font-size: var(--text-size-a);
}

.card-body-collateral {
  text-align: left;
  font-size: var(--text-size-a);
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
  width: 100%;
  height: 100%;
  min-width: 100%;
  max-width: 100%;
  border-radius: 16px;
  object-fit: contain;
}

.card-body {
  flex-basis: 50%;
}

.card-body-name {
  color: var(--text-a);
  font-size: var(--text-size-a);
  text-align: left;
  align-items: center;
  justify-content: center;
  margin-top: 1rem;
  font-weight: 400;
  white-space: normal;
  display: -webkit-box;
  -webkit-line-clamp: 1;
  -webkit-box-orient: vertical;
  overflow: hidden;
  text-overflow: ellipsis;
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

@media only screen and (max-width: 767px) {
  .grid {
    display: grid;
    border-radius: initial;
  }
}

/* Tablets and small deskbanners */
@media only screen and (min-width: 768px) and (max-width: 991px) {
  /* CSS rules for tablets and small deskbanners */
}

/* Medium deskbanners */
@media only screen and (min-width: 992px) and (max-width: 1199px) {
  /* CSS rules for medium deskbanners */
}

/* Large deskbanners and widescreen monitors */
@media only screen and (min-width: 1200px) {
  /* CSS rules for large deskbanners and widescreen monitors */
}
</style>
@/pages/home/composable/store-api
