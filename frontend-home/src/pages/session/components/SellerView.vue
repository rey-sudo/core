<template>
  <div class="stepper">
    <Toast />
    <div class="stepper-row">
      <div class="stepper-column">
        <div>1</div>
        <span></span>
      </div>
      <div class="stepper-body">
        <div class="stepper-title">Product</div>

        <div class="stepper-product">
          <img
            class="stepper-product-image"
            src="https://pisces.bbystatic.com/image2/BestBuy_US/images/products/6534/6534615_sd.jpg"
            alt=""
          />

          <div class="stepper-product-title">
            <a
              :href="'/p/' + getSlotData?.product_details.product_id"
              target="_blank"
              rel="noopener noreferrer"
            >
              {{ getSlotData?.product_details.product_name }}</a
            >

            <span> » </span>
            <span class="sku"
              >SKU: {{ getSlotData?.product_details.product_id }}</span
            >
            <span> » </span>
            <span class="model"> Model: 8430288C2C</span>
          </div>
        </div>
      </div>
    </div>

    <!--///-->
    <div class="stepper-row">
      <div class="stepper-column">
        <div>2</div>
        <span></span>
      </div>
      <div class="stepper-body">
        <div class="stepper-title">Order Information</div>
        <div class="stepper-price">
          <div>
            <label>Mode</label>
            <span>{{ getSlotData?.mode }}</span>
          </div>

          <div>
            <label>Units</label>
            <span> {{ getSlotData?.contract_units }}</span>
          </div>

          <div>
            <label>Total Price </label>
            <span>{{ getSlotData?.contract_price }} ₳ </span>
          </div>

          <div>
            <label>Total Collateral</label>
            <span>{{ getSlotData?.contract_collateral }} ₳ </span>
          </div>
        </div>
      </div>
    </div>

    <!--///////////////////////////////////////////////////////////////7-->

    <div class="stepper-row">
      <div class="stepper-column  ">
        <div>3</div>
      </div>
      <div class="stepper-body">
        <div class="stepper-buttons">
          <Button
            class="actived"
            v-if="getSlotData?.contract_0_tx"
            @click="
              createTransaction(
                'true',
                getSlotData?.id,
                getSlotData?.contract_0_utx
              )
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
              createTransaction(
                'true',
                getSlotData?.id,
                getSlotData?.contract_0_utx
              )
            "
          >
            Sign Tx2
          </Button>
        </div>
      </div>
    </div>
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
}

.stepper-buttons button {
  border: 1px solid var(--primary-c);
  background: var(--primary-c);
  font-size: var(--text-size-b);
  padding: 0.5rem 1rem;
  font-weight: 600;
  color: var(--text-w);
  margin-right: 1rem;
  border-radius: 6px;
  cursor: pointer;
}

.stepper-buttons button.actived {
  background: var(--green-a);
  border: 1px solid var(--green-a);
  pointer-events: none;
}

.sku,
.model {
  color: var(--text-b);
  font-size: var(--text-size-b);
}

.stepper-product {
  display: flex;
  align-items: center;
  width: 100%;
  margin-top: 2rem;
}

.stepper-product-image {
  width: 100px;
  height: 100px;
  object-fit: contain;
  border-radius: 6px;
  background: var(--base-a);
  border: 1px solid var(--border-b);
  padding: 0.5rem;
  overflow: hidden;
}

.stepper-product-title {
  font-size: var(--text-size-e);
  text-align: left;
  max-width: 600px;
  margin-left: 2rem;
}

.stepper-product-title a {
  color: var(--text-a);
  font-weight: 400;
}

.stepper-column {
  width: 100px;
  height: 250px;
  display: flex;
  justify-content: flex-start;
  flex-direction: column;
  align-items: center;
}

.stepper-column.short {
  height: 200px;
}

.stepper-column div {
  width: 40px;
  height: 40px;
  min-height: 40px;
  background: var(--primary-c);
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  color: var(--text-w);
  font-weight: 700;
  font-size: var(--text-size-b);
}

.stepper-column span {
  width: 2px;
  height: 100%;
  background: var(--primary-c);
}

.stepper-title {
  font-size: var(--text-size-e);
  text-align: start;
  font-weight: 500;
}

.stepper {
  background: var(--base-a); padding: 1rem;
}

.stepper-price {
  display: flex;
  align-items: center;
  text-align: start;
  margin-top: 2rem;
  max-width: 600px;
  background: var(--base-b);
  padding: 1rem;
  border-radius: 16px;
  border: 1px solid var(--border-b);
}

.stepper-price div {
  margin-right: auto;
  display: block;
}

.stepper-price div label {
  display: flex;
  align-items: baseline;
  font-size: var(--text-size-b);
  color: var(--text-b);
}

.stepper-price div span {
  line-height: 3rem;
  font-size: var(--text-size-e);
  color: var(--text-a);
  white-space: nowrap;
  text-transform: capitalize;
}

.stepper-row {
  display: flex;
  width: 100%;
}

.stepper-body {
  padding: 0.25rem;
  width: 100%;
}
</style>
