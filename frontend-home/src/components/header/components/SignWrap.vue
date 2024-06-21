<template>
  <Sidebar
    v-model:visible="visible"
    header="Cardano Wallet"
    position="right"
    :blockScroll="true"
  >
    <Steps v-model:activeStep="active" :model="items" :readonly="false" />
    <div class="grid-container">
      <div class="grid-item">1</div>
      <div class="grid-item">2</div>
      <div class="grid-item">3</div>
      <div class="grid-item">4</div>
      <div class="grid-item">5</div>
      <div class="grid-item">6</div>
      <div class="grid-item">7</div>
      <div class="grid-item">8</div>
      <div class="grid-item">9</div>
    </div>
  </Sidebar>
</template>

<script>
import { ref } from "vue";

export default {
  setup() {
    const visible = ref(true);

    const active = ref(0);

    const items = ref([
      {
        label: "Connect",
      },
      {
        label: "Select",
      },
    ]);

    return { visible, items, active };
  },
};
</script>

<style lang="css" scoped>
.grid-container {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(70px, 1fr));
  gap: 1rem;
  padding: 10px;
  margin-top: 1rem;
}

.grid-item {
  background: var(--base-b);
  color: white;
  border: 1px solid transparent;
  padding: 20px;
  text-align: center;
  border-radius: 6px;
  cursor: pointer;
  border: 1px solid var(--border-b);
}

/* Media Queries for Responsive Design */
@media (max-width: 600px) {
  .grid-item {
    font-size: 1.2em;
  }
}

@media (max-width: 400px) {
  .grid-item {
    font-size: 1em;
  }
}
</style>
