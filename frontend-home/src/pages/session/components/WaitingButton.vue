<template>
  <div class="stepper-buttons">


    <Button
      class="actived"
      v-if="getSlotData?.contract_0_tx"
      @click="
        createTransaction('true', getSlotData?.id, getSlotData?.contract_0_utx)
      "
    >
      Actived
    </Button>

    <Button
      v-if="!getSlotData?.contract_0_utx && !getSlotData?.contract_0_tx"
      @click="createTransaction('false', getSlotData?.id)"
    >
      Sign Tx1
    </Button>

    <Button
      v-if="getSlotData?.contract_0_utx && !getSlotData?.contract_0_tx"
      @click="
        createTransaction('true', getSlotData?.id, getSlotData?.contract_0_utx)
      "
    >
      Sign Tx2
    </Button>
  </div>
</template>

<script>
import { sessionAPI } from "@/pages/session/api";
import { useToast } from "primevue/usetoast";
import { headerAPI } from "@/components/header/composable/header-api";
import { balanceTx, lucidClient } from "@/api/wallet-api";
import { getAddressDetails } from "lucid-cardano";
import { walletClient } from "@/api/wallet-api";

export default {
  setup() {
    const { startTx } = headerAPI();

    const { getSlotData, startEndpoint } = sessionAPI();

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
      getSlotData,
      startEndpoint,
      showMessage,
      startTx,
    };
  },

  methods: {
    downloadTx(txHash) {
      const data = [
        ["SlotId", this.getSlotData?.id],
        ["Product", this.getSlotData?.product_details.product_name],
        ["ProductId", this.getSlotData?.product_details.product_id],
        ["Mode", this.getSlotData?.mode],
        ["Units", this.getSlotData?.contract_units],
        ["Price", this.getSlotData?.contract_price],
        ["Collateral", this.getSlotData?.contract_collateral],
        ["SlotDate", this.getSlotData?.created_at],
        ["ContractTx0", txHash],
      ];

      const csvContent = data.map((row) => row.join(",")).join("\n");

      const blob = new Blob([csvContent], { type: "text/csv;charset=utf-8;" });

      const a = document.createElement("a");
      const url = URL.createObjectURL(blob);
      a.href = url;

      const date = new Date();
      const day = String(date.getDate()).padStart(2, "0");
      const month = String(date.getMonth() + 1).padStart(2, "0");
      const year = date.getFullYear();
      const hours = date.getHours().toString().padStart(2, "0");
      const minutes = date.getMinutes().toString().padStart(2, "0");

      const dated = `${month}-${day}-${year}-${hours}-${minutes}`;

      a.download = `transaction-${this.getSlotData?.id}-${dated}.csv`;

      document.body.appendChild(a);

      a.click();

      document.body.removeChild(a);
      URL.revokeObjectURL(url);

      return txHash;
    },
    async createTransaction(actived, slotId, utx) {
      const { getWallet } = walletClient();

      lucidClient.selectWallet(getWallet());

      this.isLoading = true;

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

      if (actived === "false") {
        const addr = await lucidClient.wallet.address();
        const address = await getAddressDetails(addr);

        const params = {
          slot_id: slotId,
          seller_pubkeyhash: address.paymentCredential.hash,
        };

        await this.startEndpoint(params)
          .then((res) => balanceTx(res.response.payload.transaction))
          .then((hash) => this.downloadTx(hash))
          .then((txHash) => this.startTx({ tx_hash: txHash, slot_id: slotId }))
          .then(() => this.showMessage(successMessage))
          .catch(() => this.showMessage(errorMessage));
      }

      if (actived === "true") {
        await balanceTx(utx)
          .then((hash) => this.downloadTx(hash))
          .then((hash) => this.startTx({ tx_hash: hash, slot_id: slotId }))
          .then(() => this.showMessage(successMessage))
          .catch(() => this.showMessage(errorMessage));
      }

      this.isLoading = false;
    },
  },
};
</script>

<style lang="css" scoped>
.stepper-buttons {
  display: flex;
  margin-top: 1rem;
}

.stepper-buttons button {
  border: 1px solid var(--primary-c);
  background: var(--primary-c);
  font-size: var(--text-size-b);
  padding: 0.5rem 1rem;
  font-weight: 600;
  color: var(--text-w);
  margin-right: 1rem;
  border-radius: 4px;
  cursor: pointer;
}

.stepper-buttons button.actived {
  background: var(--green-a);
  border: 1px solid var(--green-a);
  pointer-events: none;
}


</style>
