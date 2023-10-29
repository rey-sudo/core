<template>
  <div class="m-payment">
    <PaymentModalMobile v-if="getter__viewPaymentModalMobile" />

    <div class="price">
      {{ formatPrice(getter__productData.price) }}
    </div>

    <div class="button" @click="action__viewPaymentModalMobile(true)">Comprar</div>
  </div>
</template>

<script>
import landAPI from "@/pages/land/composable/land-api";
import PaymentModalMobile from "@/pages/land/components/mobile/PaymentModalMobile";

export default {
  components: {
    PaymentModalMobile
},
  setup() {
    const {
      getter__productData,
      action__viewPaymentModalMobile,
      getter__viewPaymentModalMobile,
      formatPrice,
    } = landAPI();

    return {
      getter__productData,
      formatPrice,
      action__viewPaymentModalMobile,
      getter__viewPaymentModalMobile,
    };
  },
};
</script>

<style lang="css" scoped>
.m-payment {
  display: none;
  width: inherit;
  align-items: center;
  justify-content: space-between;
  border-bottom: 1px solid var(--border-a);
  z-index: 1;
}

.price {
  font-weight: 500;
  text-align: left;
  font-size: var(--text-size-c);
  margin: 0.5rem 1rem;
}
.button {
  padding: 0.5rem;
  background: var(--blue);
  color: var(--text-w);
  cursor: pointer;
  display: inline-block;
  text-align: center;
  white-space: nowrap;
  font-weight: 400;
  letter-spacing: -0.022em;
  min-width: 28px;
  padding-left: 16px;
  padding-right: 16px;
  padding-top: 8px;
  padding-bottom: 8px;
  border-radius: 980px;
  margin: 0.5rem 1rem;
}

@media only screen and (max-width: 768px) {
  .m-payment {
    display: flex;
  }
}
</style>
