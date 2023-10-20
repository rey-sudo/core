<template>
  <div class="p-order">
    <div v-if="isReady" class="p-order-wrap">
      <div class="p-order-wrap-top">
        <div class="image">
          <img src="./assets/drawing.svg" alt="" />
          <div class="p-order-wrap-top-title">
            <span>¡Listo! Tu paquete va en camino</span>
          </div>

          <div class="p-order-wrap-top-subtitle">
            <span>ID: {{ this.getter__orderData.pid }}</span>
          </div>
          <div class="p-order-wrap-top-timeline">
            <TimelineComp />
          </div>
        </div>
      </div>
      <div class="p-order-wrap-bottom"></div>
    </div>

    <LoaderWrap v-if="!isReady" />
  </div>
</template>

<script>
import TimelineComp from "@/pages/order/components/TimelineComp.vue";
import orderAPI from "@/pages/order/composable/order-api";
import LoaderWrap from "@/components/LoaderWrap.vue";
import { useRouter } from "vue-router";
import { ref } from "vue";

export default {
  components: {
    TimelineComp,
    LoaderWrap,
  },
  setup() {
    const router = useRouter();

    const { action__getOrderData, getter__orderData } = orderAPI();

    const isReady = ref(false);

    return { router, action__getOrderData, isReady, getter__orderData };
  },
  created() {
    this.$watch(
      () => this.$route.params,
      (e) => {
        const params = {
          pid: e.pid,
        };

        this.action__getOrderData(params)
          .then(() => {
            this.router.replace({
              params: {
                name: this.getter__orderData.product[0].name.replace(/\s+/g, "-"),
              },
            });

            this.isReady = true;
          })
          .catch((e) => console.error(e));
      },
      { immediate: true }
    )();
  },
};
</script>

<style lang="css" scoped>
img {
  animation: bounce 2s ease-in-out infinite;
}

@keyframes bounce {
  0%,
  20%,
  50%,
  80%,
  100% {
    transform: translateY(0) rotate(0deg);
  }
  40% {
    transform: translateY(-20px) rotate(-5deg);
  }
  60% {
    transform: translateY(-10px) rotate(5deg);
  }
}
.p-order-wrap-top-timeline {
  position: fixed;
  top: 48%;
}
.image {
  width: 1000px;
  height: 100%;
  position: relative;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
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

.p-order-wrap-top-subtitle {
  color: var(--text-w);
  font-size: var(--text-size-c);
  margin-top: 1rem;
}

.p-order-wrap-bottom {
  height: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
}
</style>
