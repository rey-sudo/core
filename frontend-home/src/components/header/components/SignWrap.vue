<template>
  <Sidebar
    v-model:visible="isVisible"
    header="Setup Wallet"
    position="right"
    blockScroll
    :dismissable="false"
  >
    <div class="steps">
      <div
        class="steps-item"
        v-for="(item, index) in stepsList"
        :key="item"
        :class="{ active: activeStep === index }"
      >
        <i :class="item.icon" />

        <div class="steps-legend">{{ item.label }}</div>
      </div>
    </div>

    <div class="tab-1" v-if="activeStep === 0">
      <p class="message">
        Select the Cardano wallet of your preference. Please note that each
        wallet can have one or more accounts. If you are going to make purchase
        operations you must use a single account throughout the entire process.
      </p>

      <div class="grid-container">
        <div @click="selectWallet('nami')" class="grid-item">
          <img src="@/assets/nami.svg" alt="logo" />
        </div>
        <div @click="selectWallet('eternl')" class="grid-item">
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

    <div class="tab-2" v-if="activeStep === 1">
      <div class="account">SELLER</div>
    </div>
  </Sidebar>
</template>

<script>
import { ref, watch } from "vue";
import { walletClient } from "@/api/wallet-api";
import { headerAPI } from "../composable/header-api";

export default {
  setup() {
    const { getSetupWallet, setupWallet } = headerAPI();

    const isVisible = ref(false);

    const activeStep = ref(0);

    const checkStorate = localStorage.getItem("pairfy-wallet");

    if (checkStorate !== null) {
      activeStep.value = 1;
    } else {
      isVisible.value = true;
    }

    window.addEventListener("walletEnabledEvent", () => {
      activeStep.value = 1;
    });

    watch(getSetupWallet, (newValue) => {
      if (newValue === true) {
        isVisible.value = true;
      }
    });

    watch(isVisible, (newValue) => {
      if (newValue === false) {
        setupWallet(false);
      }
    });
    const wallet = walletClient();

    const stepsList = ref([
      {
        label: "Connect",
        icon: "pi pi-wallet",
      },
      {
        label: "Account",
        icon: "pi pi-user",
      },
    ]);

    const selectWallet = async (e) => {
      await wallet.connect(e);
    };

    return {
      isVisible,
      stepsList,
      activeStep,
      wallet,
      selectWallet,
      getSetupWallet,
      setupWallet,
    };
  },
};
</script>

<style lang="css" scoped>
.steps {
  padding: 1rem 2rem;
  display: flex;
  align-items: baseline;
  justify-content: space-between;
}

.steps-item {
  border: 1px solid var(--border-b);
  border-radius: 6px;
  width: 50px;
  height: 50px;
  display: flex;
  justify-content: center;
  align-items: center;
  cursor: pointer;
}

.steps-item i {
  font-size: var(--text-size-d);
}

.steps-item.active {
  border: 1px solid var(--blue-c);
  color: var(--blue-c);
}

.steps-legend {
  position: absolute;
  margin-top: 90px;
  font-size: var(--text-size-b);
  font-weight: 500;
}

.message {
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

.tab-1 {
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
