<template>
  <div class="shipping">
    <div class="subtitle">Press the button when you send the package.</div>

    <Button @click="shippingTransaction"> Shipped </Button>
  </div>
</template>

<script>
import headerAPI from "@/components/header/composable/header-api";
import { sessionAPI } from "@/pages/session/api";
import { walletClient, balanceTx, lucidClient } from "@/api/wallet-api";
import { getAddressDetails } from 'lucid-cardano';

export default {
  setup() {
    const { startTx } = headerAPI();

    const { getOrderData, shipping, shippingTx } = sessionAPI();


    return {
      getOrderData,
      shipping,
      startTx,
      shippingTx
    };
  },

  methods: {
    async shippingTransaction() {
      const { getWallet } = walletClient();

      lucidClient.selectWallet(await getWallet());

      const addr = await lucidClient.wallet.address();
      const address = await getAddressDetails(addr);

      await this.shipping({
        order_id: this.getOrderData.id,
        address: address.address.bech32,
      }).then((res) => balanceTx(res.response.payload.transaction))
        .then((hash) => this.shippingTx({ tx_hash: hash, order_id: this.getOrderData.id }))
        .then(() =>
          this.$toast.add({
            severity: "success",
            summary: "Successful",
            detail: "Transaction sent to the network.",
            life: 5000,
          })
        )
        .catch((err) => {
          if (err.response?.errors) {
            return this.$toast.add({
              severity: "error",
              summary: "Error Message",
              detail: err.response.errors[0].message,
              life: 5000,
            })
          }

          this.$toast.add({
            severity: "error",
            summary: "Error Message",
            detail: "Transaction Failed",
            life: 5000,
          })
        });
    },
  },
};
</script>

<style lang="css" scoped>
.shipping {
  display: flex;
  flex-direction: column;
}

.shipping button {
  border: 1px solid var(--primary-c);
  background: var(--primary-c);
  font-size: var(--text-size-b);
  padding: 0.5rem 1rem;
  width: 100px;
  justify-content: center;
  font-weight: 600;
  color: var(--text-w);
  margin-right: 1rem;
  border-radius: 4px;
  cursor: pointer;
  margin-top: 1rem;
}

.shipping button.actived {
  background: var(--primary-c);
  border: 1px solid var(--primary-c);
  pointer-events: none;
}

.subtitle {
  font-size: var(--text-size-b);
  text-align: left;
  margin-top: 0.5rem;
}
</style>
