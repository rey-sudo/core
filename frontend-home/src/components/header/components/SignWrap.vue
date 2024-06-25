<template>
  <Sidebar
    v-model:visible="visible"
    header="Setup Wallet"
    position="right"
    blockScroll
    :dismissable="false"
  >
    <Steps v-model:activeStep="activeStep" :model="steps" readonly />
    <div class="stepOne" v-if="activeStep === 0">
      <p class="legend">
        Select the Cardano wallet of your preference. Please note that each
        wallet can have one or more accounts. If you are going to make purchase
        operations you must use a single account throughout the entire process.
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
    </div>

    <div class="stepTwo" v-if="activeStep === 1">
      <div class="account">SELLER</div>
    </div>
  </Sidebar>
</template>

<script>
import { ref, watch } from "vue";
import { walletAPI } from "@/api/wallet-api";
import { headerAPI } from "../composable/header-api";

export default {
  setup() {
    const { getSetupWallet, setupWallet } = headerAPI();

    const visible = ref(false);  

    watch(getSetupWallet, (newValue) => {
      if (newValue === true) {
        visible.value = true;
      }
    });

    watch(visible, (newValue) => {
      if (newValue === false) {
        setupWallet(false);
      }
    });
    const wallet = walletAPI();

    const activeStep = ref(1);

    const steps = ref([
      {
        label: "Connect",
      },
      {
        label: "Account",
      },
    ]);

    const connectWallet = (e) => {
      wallet.connect(e);
    };

    return {
      visible,
      steps,
      activeStep,
      wallet,
      connectWallet,
      getSetupWallet,
      setupWallet,
    };
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
  font-size: var(--text-size-b);
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
  width: 34px;
  height: 34px;
}

.stepOne {
}

.account {
  border: 1px solid var(--border-b);
  padding: 1rem;
  border-radius: 6px;
  margin-top: 1rem;
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
