<template>
  <div class="p-order">
    <div class="p-order-wrap">
      <TimelineComp />
    </div>
  </div>
</template>

<script>
import TimelineComp from "@/pages/order/components/TimelineComp.vue";

export default {
  components: {
    TimelineComp,
  },
};
</script>

<style lang="css" scoped>
.p-order {
  width: 100%;
  height: 100vh;
  background: var(--green);
  display: flex;
  justify-content: center;
  align-items: flex-end;
}

.p-order-wrap {
  width: 100%;
  height: 60%;
  background: var(--base-a);
}
</style>
