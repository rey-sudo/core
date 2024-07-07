<template>
  <div class="icons">
    <div v-for="item in items" :key="item">
      <i :class="item.icon" />
      <span>{{ item.label }}</span>
    </div>
  </div>
</template>

<script>
import { ref } from "vue";

export default {
  setup() {
    const items = ref([
      {
        label: "Collateral 500 ADA",
        icon: "pi pi-credit-card",
      },
      {
        label: "Treasury discount 30 ADA",
        icon: "pi pi-building-columns",
      },
      {
        label: "Protected purchase 100%",
        icon: "pi pi-shopping-cart",
      },
    ]);

    return { items };
  },
};
</script>

<style lang="css" scoped>
.icons {
  margin-top: 1rem;
  display: flex;
  flex-direction: column;
}

.icons div {
  display: flex;
  align-items: center;
  margin-top: 1rem;
}

.icons span {
  margin-left: 0.5rem;
  font-size: var(--text-size-b);
  color: var(--text-b);
}

.icons i {
  color: var(--primary-a);
}
</style>
