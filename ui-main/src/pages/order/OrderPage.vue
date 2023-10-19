<template>
  <div class="p-order">
    <div class="p-order-wrap">
      <div class="p-order-wrap-top">
        <img src="./assets/drawing.svg" alt="" />
        <div class="p-order-wrap-top-title">
          <span>¡Listo! Tu paquete va en camino</span>
        </div>

        <div class="p-order-wrap-top-subtitle">
          <span>Orden: 8HHD73HZJC</span>
        </div>
      </div>

      <div class="p-order-wrap-bottom">
        <TimelineComp />
      </div>
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
img{
  animation: bounce 2s ease-in-out infinite;
}

@keyframes bounce {
  0%, 20%, 50%, 80%, 100% {
    transform: translateY(0) rotate(0deg);
  }
  40% {
    transform: translateY(-20px) rotate(-5deg);
  }
  60% {
    transform: translateY(-10px) rotate(5deg);
  }
}

.p-order {
  width: 100%;
  height: 100vh;
  display: flex;
  justify-content: center;
  align-items: flex-end;
}

.p-order-wrap {
  width: 100%;
  height: 100%;
  background: var(--base-a);
}

.p-order-wrap-top {
  height: 50%;
  background: var(--green);
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
}

.p-order-wrap-top-title {
  color: var(--text-w);
  font-size: var(--text-size-d);
  font-weight: 600;
  margin-top: 2rem;
}

.p-order-wrap-top-subtitle{
  color: var(--text-w);
  font-size: var(--text-size-c);
  margin-top: 1rem;
}

.p-order-wrap-bottom{
  height: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
}
</style>
