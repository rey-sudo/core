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
            <label>Type <i class="pi pi-info-circle" /></label>
            <span>Batch</span>
          </div>

          <div>
            <label
              >Price

              <i class="pi pi-info-circle" />
            </label>
            <span>3.240 ADA</span>
          </div>

          <div>
            <label>Collateral <i class="pi pi-info-circle" /></label>
            <span>500 ADA</span>
          </div>

          <div>
            <label>Discount <i class="pi pi-info-circle" /></label>
            <span>40 ADA</span>
          </div>

          <div>
            <label>Units <i class="pi pi-info-circle" /></label>
            <span>2</span>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup></script>

<style lang="css" scoped>
.stepper-column {
  width: 100px;
  height: 200px;
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
  width: 4px;
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
  color: var(--text-a);
}

.stepper-price div label i {
  margin-left: 0.5rem;
  font-size: var(--text-size-a);
}

.stepper-price div span {
  line-height: 3rem;
  font-size: var(--text-size-e);
  color: var(--text-a);
}

.stepper-row {
  display: flex;
}

.stepper-body {
  padding: 1rem;
}
</style>
