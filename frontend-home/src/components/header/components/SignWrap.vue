<template>
  <Sidebar
    v-model:visible="visible"
    header="Setup Cardano Wallet"
    position="right"
    :blockScroll="true"
  >
    <Steps v-model:activeStep="active" :model="items" :readonly="false" />
    <div class="grid-container">
      <div class="grid-item"><img src="@/assets/nami.svg" alt="logo" /></div>
      <div class="grid-item"><img src="@/assets/eternl.png" alt="logo" /></div>
      <div class="grid-item"></div>
      <div class="grid-item"></div>
      <div class="grid-item"></div>
      <div class="grid-item"></div>
      <div class="grid-item"></div>
      <div class="grid-item"></div>
      <div class="grid-item"></div>
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
  grid-template-columns: repeat(auto-fit, minmax(60px, 1fr));
  gap: 1rem;
  padding: 1rem;
  margin-top: 1rem;
}

.grid-item {
  background: var(--base-b);
  border: 1px solid transparent;
  text-align: center;
  height: 60px;
  width: 60px;
  border-radius: 6px;
  cursor: pointer;
  border: 1px solid var(--border-b);
  display: flex;
  align-items: center;
  justify-content: center;
  transition: var(--transition-a);
}

.grid-item:hover {
  border: 1px solid var(--blue-c);
}
.grid-item img {
  width: 40px;
  height: 40px;
}

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
