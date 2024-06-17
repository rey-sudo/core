<template>
  <div class="stepper">
    <div class="stepper-row">
      <div class="stepper-column">
        <div>1</div>
        <span></span>
      </div>
      <div class="stepper-body">
        <div class="stepper-title">Order Information</div>
        <div class="stepper-price">
          <div>
            <label>Type</label>
            <span>Batch</span>
          </div>

          <div>
            <label>Price </label>
            <span>3.240 ADA</span>
          </div>

          <div>
            <label>Collateral</label>
            <span>500 ADA</span>
          </div>

          <div>
            <label>Discount </label>
            <span>40 ADA</span>
          </div>

          <div>
            <label>Units</label>
            <span>2</span>
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
        <div class="stepper-title">Product</div>

        <div class="stepper-product">
          <img
            class="stepper-product-image"
            src="https://pisces.bbystatic.com/image2/BestBuy_US/images/products/6534/6534615_sd.jpg"
            alt=""
          />

          <div class="stepper-product-title">
            Apple - MacBook Pro 14" Laptop - M3 Pro chip - 18GB Memory - 14-core
            GPU - 512GB SSD - Space Black

            <span> | </span>
            <span class="sku">SKU: P9C3KC93CK</span>
            <span> | </span>
            <span class="model"> Model: 8430288C2C</span>
          </div>
        </div>
      </div>
    </div>

    <!--///-->

    <div class="stepper-row">
      <div class="stepper-column">
        <div>3</div>
      </div>
      <div class="stepper-body">
        <div class="stepper-title">Order Information</div>
      </div>
    </div>
  </div>
</template>

<script setup></script>

<style lang="css" scoped>
.sku,
.model {
  color: var(--text-b);
  font-size: var(--text-size-a);
}
.stepper-product {
  display: flex;
  align-items: center;
  width: 100%;
  margin-top: 1rem;
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
  font-weight: 500;
  max-width: 500px;
  padding: 1rem;
}

.stepper-column {
  width: 100px;
  height: 250px;
  display: flex;
  justify-content: start;
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
  width: 3px;
  height: 100%;
  background: var(--blue-c);
}

.stepper-title {
  font-size: var(--text-size-e);
  text-align: start;
  font-weight: 600;
}

.stepper {
  background: var(--base-a);
  margin: 1rem 0;
}

.stepper-price {
  display: flex;
  align-items: center;
  text-align: start;
  margin-top: 2rem;
  border-radius: 8px;
}

.stepper-price div {
  margin-right: 4rem;
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
}

.stepper-row {
  display: flex;
  width: 100%;
}

.stepper-body {
  padding: 0.5rem;
  width: 100%;
}
</style>
