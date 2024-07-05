<template>
  <div class="stepper">
    <Toast />
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
      <div class="stepper-column short">
        <div>2</div>
        <span></span>
      </div>
      <div class="stepper-body">
        <div class="stepper-title">Information</div>
        <div class="stepper-price">
          <div>
            <label>Total Price </label>
            <span>{{ getSlotData?.contract_price }} ADA </span>
          </div>

          <div>
            <label>Total Collateral</label>
            <span>{{ getSlotData?.contract_collateral }} ADA </span>
          </div>

          <div>
            <label>Mode</label>
            <span>{{ getSlotData?.mode }}</span>
          </div>

          <div>
            <label>Units</label>
            <span> {{ getSlotData?.contract_units }}</span>
          </div>
        </div>
      </div>
    </div>

    <!--///////////////////////////////////////////////////////////////7-->

    <div class="stepper-row">
      <div class="stepper-column">
        <div>3</div>
      </div>
      <div class="stepper-body">
        <div class="stepper-title">Action</div>
        <WaitingButton
          v-if="
            getSlotData?.contract_stage === 'inactive' ||
            getSlotData?.contract_stage === 'start' ||
            getSlotData?.contract_stage === 'waiting'
          "
        />
      </div>
    </div>
  </div>
</template>

<script>
import { sessionAPI } from "@/pages/session/api";
import WaitingButton from "./WaitingButton.vue";

export default {
  components: {
    WaitingButton,
  },
  setup() {
    const { getSlotData } = sessionAPI();

    return {
      getSlotData,
    };
  },
};
</script>

<style lang="css" scoped>
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
  border: 1px solid var(--border-b);
  padding: 0.5rem;
  overflow: hidden;
}

.stepper-product-title {
  font-size: var(--text-size-d);
  text-align: left;
  max-width: 600px;
  margin-left: 2rem;
  line-height: 2rem;
}

.stepper-product-title a {
  color: var(--text-a);
  font-weight: 400;
}

.stepper-column {
  width: calc(40px + 2rem);
  height: 250px;
  display: flex;
  justify-content: flex-start;
  flex-direction: column;
  align-items: center;
}

.stepper-column.short {
  height: 200px;
}

.stepper-column div {
  width: 40px;
  height: 40px;
  min-height: 40px;
  background: var(--primary-c);
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
  background: var(--primary-c);
}

.stepper-title {
  font-size: var(--text-size-e);
  text-align: start;
  font-weight: 500;
}

.stepper {
  background: var(--base-a);
  padding: 1rem;
}

.stepper-price {
  display: flex;
  align-items: center;
  text-align: start;
  margin-top: 2rem;
  max-width: 500px;
  border-radius: 16px;
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
  font-size: var(--text-size-d);
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
