<template>
  <Sidebar
    v-model:visible="isVisible"
    header="Setup Wallet"
    position="right"
    blockScroll
    :dismissable="true"
  >
    <div class="steps">
      <div
        class="steps-item"
        v-for="(item, index) in stepsList"
        :key="item"
        :class="{ active: activeStep === index }"
        @click="handleStep(index)"
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
        <div
          @click="selectWallet('nami')"
          class="grid-item"
          :class="{ active: enabledWallet === 'nami' }"
        >
          <img src="@/assets/nami.svg" alt="nami" />
        </div>
        <div
          @click="selectWallet('eternl')"
          class="grid-item"
          :class="{ active: enabledWallet === 'eternl' }"
        >
          <img src="@/assets/eternl.png" alt="eternl" />
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
      <div class="account">
        <div class="selector">
          <div
            @click="selectTab('user')"
            :class="{ active: activeTab === 'user' }"
          >
            User
          </div>
          <div
            @click="selectTab('seller')"
            :class="{ active: activeTab === 'seller' }"
          >
            Seller
          </div>
        </div>

        <SellerLogin />
      </div>
    </div>
  </Sidebar>
</template>

<script>
import { ref, watch } from "vue";
import { walletClient } from "@/api/wallet-api";
import { headerAPI } from "../composable/header-api";
import SellerLogin from "./SellerLogin.vue";

export default {
  components: {
    SellerLogin,
  },
  setup() {
    const { getSetupWallet, setupWallet } = headerAPI();

    const isVisible = ref(true);

    const activeStep = ref(0);

    const enabledWallet = ref(null);

    const getWalletName = () => localStorage.getItem("pairfy-wallet");

    if (getWalletName() !== null) {
      activeStep.value = 1;
      enabledWallet.value = getWalletName();
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

    const handleStep = (e) => {
      activeStep.value = e;
      enabledWallet.value = getWalletName();
    };

    const activeTab = ref("seller");

    const selectTab = (e) => {
      activeTab.value = e;
    };

    return {
      isVisible,
      stepsList,
      selectTab,
      activeStep,
      activeTab,
      enabledWallet,
      handleStep,
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
  padding: 1rem 4rem;
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

.grid-item.active {
  border: 1px solid var(--blue-c);
}

.tab-1 {
}

.account {
  border-radius: 12px;
  margin-top: 2rem;
  display: flex;
  flex-direction: column;
  border: 1px solid var(--border-b);
  min-height: 600px;
}

.selector {
  display: flex;
  background: var(--base-b);
  padding: 0.25rem;
  overflow: hidden;
  border-radius: 12px;
}

.selector div {
  padding: 0.75rem;
  text-align: center;
  width: 100%;
  cursor: pointer;
  font-size: var(--text-size-c);
  font-weight: 400;
  border-radius: 6px;
  transition: 0.3s ease-in-out;
}

.selector div.active {
  font-weight: 500;
  color: var(--blue-a);
  background: var(--base-a);
  box-shadow: var(--shadow-a);
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
