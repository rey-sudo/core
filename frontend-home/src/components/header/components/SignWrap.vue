<template>
  <Sidebar
    v-model:visible="visible"
    header="Setup Wallet"
    position="right"
    :blockScroll="true"
  >
    <Steps v-model:activeStep="active" :model="items" :readonly="false" />

    <p class="legend">
      Select the cardano wallet of your preference. Please note that each wallet
      can have one or more accounts. If you are going to make purchase
      operations, you must use a single account throughout the entire process.
    </p>

    <div class="grid-container">
      <div @click="connectWallet('nami')" class="grid-item">
        <img src="@/assets/nami.svg" alt="logo" />
      </div>
      <div @click="connectWallet('eternl')" class="grid-item">
        <img src="@/assets/eternl.png" alt="logo" />
      </div>
      <div class="grid-item"></div>
      <div class="grid-item"></div>
      <div class="grid-item"></div>
      <div class="grid-item"></div>
      <div class="grid-item"></div>
      <div class="grid-item"></div>
      <div class="grid-item"></div>
    </div>
  </Sidebar>
</template>

<script>
import { ref } from "vue";
import { walletAPI } from "@/api/wallet-api";

export default {
  setup() {
    const visible = ref(true);

    const wallet = walletAPI();

    const active = ref(0);

    const items = ref([
      {
        label: "Connect",
      },
      {
        label: "Select",
      },
    ]);

    const connectWallet = (e) => {
      wallet.connect(e);
    };

    return { visible, items, active, wallet, connectWallet };
  },
};
</script>

<style lang="css" scoped>
.legend {
  padding: 1rem;
  border: 1px solid var(--border-b);
  border-radius: 12px;
  margin-top: 2rem;
  background: var(--base-b);
  line-height: 1.75rem;
}

.grid-container {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(60px, 1fr));
  gap: 1rem;
  padding: 1rem;
  margin-top: 1rem;
}

.grid-item {
  background: var(--base-b);
  border: 1px solid transparent;
  text-align: center;
  height: 60px;
  width: 60px;
  border-radius: 6px;
  cursor: pointer;
  border: 1px solid var(--border-b);
  display: flex;
  align-items: center;
  justify-content: center;
  transition: var(--transition-a);
}

.grid-item:hover {
  border: 1px solid var(--blue-c);
}
.grid-item img {
  width: 40px;
  height: 40px;
}

@media (max-width: 600px) {
  .grid-item {
    font-size: 1.2em;
  }
}

@media (max-width: 400px) {
  .grid-item {
    font-size: 1em;
  }
}
</style>
