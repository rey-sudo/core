<template>
  <div class="waiting">
    <!--//////////////////////////////////////////////////////////////////////-->

    <template v-if="getOrderData.contract_0_tx">
      <div class="subtitle">The transaction has been sent to the network.</div>

      <Button class="actived">
        <span>Actived</span>

        <span style="margin-left: 0.5rem;"> <i class="pi pi-check" style="font-size: 12px;" /></span>
      </Button>
    </template>
    <!--//////////////////////////////////////////////////////////////////////-->
    <template v-if="!getOrderData.contract_0_tx">
      <div class="subtitle">Press to activate the script on the network.</div>

      <Button @click="createTransaction(getOrderData.id)">
        <span>Deploy</span>
      </Button>
    </template>
    <!--//////////////////////////////////////////////////////////////////////-->
  </div>
</template>

<script>
import { sessionAPI } from "@/pages/session/api";
import { useToast } from "primevue/usetoast";
import headerAPI from "@/components/header/composable/header-api";
import { balanceTx, lucidClient } from "@/api/wallet-api";
import { getAddressDetails } from "lucid-cardano";
import { walletClient } from "@/api/wallet-api";

export default {
  setup() {
    const { startTx } = headerAPI();

    const { getOrderData, deploy, deployTx } = sessionAPI();

    const toast = useToast();

    const showMessage = ({ severity, summary, detail, life }) => {
      toast.add({
        severity,
        summary,
        detail,
        life,
      });
    };

    return {
      getOrderData,
      deploy,
      showMessage,
      startTx,
      deployTx
    };
  },

  methods: {
    async createTransaction(slotId) {
      this.isLoading = true;

      const { getWallet } = walletClient();

      lucidClient.selectWallet(await getWallet());

      const successMessage = {
        severity: "success",
        summary: "Successful",
        detail: "Transaction sent to the network.",
        life: 5000,
      };

      const errorMessage = {
        severity: "error",
        summary: "Error Message",
        detail: "Transaction canceled.",
        life: 5000,
      };

      const addr = await lucidClient.wallet.address();
      const address = await getAddressDetails(addr);

      await this.deploy({
        order_id: slotId,
        address: address.address.bech32,
        pubkeyhash: address.paymentCredential.hash,
      }).then((res) => balanceTx(res.response.payload.transaction))
        .then((hash) => this.deployTx({ tx_hash: hash, order_id: slotId }))
        .then(() =>
          this.$toast.add(successMessage)
        )
        .catch(() =>
          this.$toast.add(errorMessage)
        );

      this.isLoading = false;
    },
  },
};
</script>

<style lang="css" scoped>
.waiting {
  display: flex;
  flex-direction: column;
}

.waiting button {
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

.waiting button.actived {
  background: var(--primary-c);
  border: 1px solid var(--primary-c);
  pointer-events: none;
  cursor: not-allowed;
}

.subtitle {
  font-size: var(--text-size-c);
  text-align: left;
  margin-top: 0.5rem;
}
</style>
