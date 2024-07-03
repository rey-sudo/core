<template>
  <div class="stepper">
    <div class="stepper-row">
      <div class="stepper-column">
        <div>1</div>
        <span></span>
      </div>
      <div class="stepper-body">
        <div class="stepper-title">Product</div>

        <div class="stepper-product">
          <img
            class="stepper-product-image"
            src="https://pisces.bbystatic.com/image2/BestBuy_US/images/products/6534/6534615_sd.jpg"
            alt=""
          />

          <div class="stepper-product-title">
            <a
              :href="'/p/' + getSlotData?.product_details.product_id"
              target="_blank"
              rel="noopener noreferrer"
            >
              {{ getSlotData?.product_details.product_name }}</a
            >

            <span> » </span>
            <span class="sku"
              >SKU: {{ getSlotData?.product_details.product_id }}</span
            >
            <span> » </span>
            <span class="model"> Model: 8430288C2C</span>
          </div>
        </div>
      </div>
    </div>

    <!--///-->
    <div class="stepper-row">
      <div class="stepper-column">
        <div>2</div>
        <span></span>
      </div>
      <div class="stepper-body">
        <div class="stepper-title">Order Information</div>
        <div class="stepper-price">
          <div>
            <label>Mode</label>
            <span>{{ getSlotData?.mode }}</span>
          </div>

          <div>
            <label>Units</label>
            <span> {{ getSlotData?.contract_units }}</span>
          </div>

          <div>
            <label>Total Price </label>
            <span>{{ getSlotData?.contract_price }} ₳ </span>
          </div>

          <div>
            <label>Total Collateral</label>
            <span>{{ getSlotData?.contract_collateral }} ₳ </span>
          </div>
        </div>
      </div>
    </div>

    <!--///-->

    <!--///-->

    <div class="stepper-row">
      <div class="stepper-column">
        <div>3</div>
      </div>
      <div class="stepper-body">
        <div class="stepper-buttons">
          <button v-if="getSlotData?.contract_stage === 'inactive'">
            Active
          </button>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import { sessionAPI } from "@/pages/session/api";

export default {
  setup() {
    const { getSlotData } = sessionAPI();

    return {
      getSlotData,
    };
  },
};
</script>

<style lang="css" scoped>
.stepper-buttons {
  display: flex;
}

.stepper-buttons button {
  border: 1px solid var(--blue-c);
  background: var(--blue-c);
  padding: 0.5rem 1rem;
  font-weight: 600;
  color: var(--text-w);
  margin-right: 1rem;
  border-radius: 6px;
  cursor: pointer;
}

.sku,
.model {
  color: var(--text-b);
  font-size: var(--text-size-b);
}
.stepper-product {
  display: flex;
  align-items: center;
  width: 100%;
  margin-top: 2rem;
}

.stepper-product-image {
  width: 100px;
  height: 100px;
  object-fit: contain;
  border-radius: 6px;
  background: var(--base-a);
  border: 1px solid var(--border-a);
  padding: 0.5rem;
  overflow: hidden;
}

.stepper-product-title {
  font-size: var(--text-size-e);
  text-align: left;
  max-width: 600px;
  padding-left: 2rem;
}

.stepper-product-title a {
  color: var(--text-a);
  font-weight: 400;
}

.stepper-column {
  width: 100px;
  height: 250px;
  display: flex;
  justify-content: flex-start;
  flex-direction: column;
  align-items: center;
}

.stepper-column div {
  width: 40px;
  height: 40px;
  min-height: 40px;
  background: var(--blue-c);
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  color: var(--text-w);
  font-weight: 700;
  font-size: var(--text-size-b);
}

.stepper-column span {
  width: 2px;
  height: 100%;
  background: var(--blue-c);
}

.stepper-title {
  font-size: var(--text-size-f);
  text-align: start;
  font-weight: 500;
}

.stepper {
  background: var(--base-a);
}

.stepper-price {
  display: flex;
  align-items: center;
  text-align: start;
  margin-top: 2rem;
  max-width: 600px;
}

.stepper-price div {
  margin-right: auto;
  display: block;
}

.stepper-price div label {
  display: flex;
  align-items: baseline;
  font-size: var(--text-size-b);
  color: var(--text-b);
}

.stepper-price div span {
  line-height: 3rem;
  font-size: var(--text-size-e);
  color: var(--text-a);
  white-space: nowrap;
  text-transform: capitalize;
}

.stepper-row {
  display: flex;
  width: 100%;
}

.stepper-body {
  padding: 0.25rem;
  width: 100%;
}
</style>
