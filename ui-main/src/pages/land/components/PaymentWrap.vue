<template>
  <div class="p-payment">
    <PaymentModal v-if="getter__viewPaymentModal" />
    <div class="p-discount">
      {{ getter__productData.discount_label }}
    </div>

    <div class="card-body-diff">
      <span>
        {{ formatPrice(getter__productData.price_diff) }}
      </span>
    </div>

    <div class="card-body-price">
      <span class="dollar">$ </span>
      <span> {{ formatPrice(getter__productData.price) }} </span>
    </div>

    <div class="card-body-payment">
      {{ getter__productData.payment_type }}
    </div>

    <div
      class="card-body-stock"
      :class="{
        blue: getter__productData.stock_supply,
        red: !getter__productData.stock_supply,
      }"
    >
      <span> {{ getter__productData.stock_supply }} Disponible</span>
    </div>

    <div
      class="card-body-shipping"
      :class="{
        green: !getter__productData.shipping_tax,
        gray: getter__productData.shipping_tax,
      }"
    >
      <span v-if="!shipping_tax">
        {{ getter__productData.shipping_label }} <i class="pi pi-bolt"/> </span
      >
    </div>

    <div class="p-payment-button top" @click="action__viewPaymentModal(true)">
      Pagar contraentrega
    </div>
    <div class="p-payment-button bottom">Con otros medios</div>
  </div>
</template>

<script>
import landAPI from "@/pages/land/composable/land-api";
import PaymentModal from "@/pages/land/components/PaymentModal";

export default {
  components: {
    PaymentModal,
  },
  setup() {
    const {
      getter__productData,
      action__viewPaymentModal,
      getter__viewPaymentModal,
    } = landAPI();

    return {
      getter__productData,
      action__viewPaymentModal,
      getter__viewPaymentModal,
    };
  },

  methods: {
    formatPrice(num) {
      const price = num || 0;

      const formattedPrice = price.toLocaleString("en-US", {
        style: "currency",
        currency: "COP",
      });

      return formattedPrice;
    },
  },
};
</script>

<style lang="css" scoped>
.p-payment {
  display: flex;
  flex-direction: column;
  width: 80%;
}

.card-body-price {
  font-weight: 600;
  text-align: left;
  font-size: var(--text-size-d);
}

.card-body-diff {
  font-weight: 400;
  text-align: left;
  margin-top: 2rem;
  font-size: var(--text-size-b);
  text-decoration: line-through;
  color: var(--text-b);
}

.p-discount {
  width: 80px;
  font-size: var(--text-size-a);
  color: var(--text-w);
  background: black;
  border-radius: 999px;
  padding: 0.25rem 0rem;
}

.card-body-payment {
  color: var(--text-b);
  font-size: var(--text-size-b);
  text-align: left;
  font-weight: initial;
  margin-top: 2rem;
  text-transform: capitalize;
}

.card-body-stock {
  margin-top: 0.75rem;
  text-align: left;
  font-size: var(--text-size-b);
}

.card-body-shipping {
  margin-top: 0.75rem;
  text-align: left;
  font-size: var(--text-size-b);
  font-weight: 600;
}

.p-payment-button {
  padding: 1rem;
  border-radius: 999px;
  font-weight: 500;
  cursor: pointer;
  margin-top: 1rem;
}
.p-payment-button.top {
  background: var(--blue);
  color: var(--text-w);
}

.p-payment-button.bottom {
  border: 1px solid var(--border-a);
  color: var(--text-a);
}
</style>
