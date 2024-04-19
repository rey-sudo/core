<template>
  <header class="header mobile">
    <div class="header-left">
      <img
        class="header-left-logo"
        @click="reloadPage"
        src="@/assets/logo.png"
        alt=""
      />

      <div class="header-button left">
        <label for=""> <img src="@/assets/location.svg" alt="" /></label>
        <div>
          <span>EN</span>
          <span>🇺🇸</span>
        </div>
      </div>

      <div class="header-button left">
        <label for=""> <img src="@/assets/store.svg" alt="" /></label>
        <div>
          <span>Sell a</span>
          <span>Product</span>
        </div>
      </div>
    </div>

    <div class="header-center">
      <div class="header-center-search">
        <input
          type="text"
          maxlength="200"
          placeholder="What do you need to find today?"
        />

        <div class="header-center-search-icon">
          <i class="pi pi-search" />
        </div>
      </div>
    </div>

    <div class="header-right">
      <div class="header-button right">
        <label for=""> <img src="@/assets/gift.svg" alt="" /></label>
        <div>
          <span>Be a</span>
          <span>Mediator</span>
        </div>
      </div>

      <div class="header-button right">
        <label for=""> <img src="@/assets/cart.svg" alt="" /></label>

        <div>
          <span class="counter">0</span>
          <span>Items</span>
        </div>
      </div>

      <div data-v-4f11bcd6="" class="header-button right">
        <button class="wallet-button" data-v-4f11bcd6="" @click="connectWallet">
          <i class="pi pi-bars" />
          <img src="@/assets/user.svg" alt="" />
        </button>
      </div>
    </div>

    <!--SUBMENU-->

    <div class="submenu" :class="{ blue: currentRoute === 'product' }">
      <div class="submenu-column left">
        <div class="submenu-button">
          <i class="pi pi-bars" />
        </div>

        <div class="submenu-nav">
          <div
            v-for="item in navTabs"
            :key="item"
            :class="{ active: selectedTab === item.value }"
          >
            {{ item.label }}
          </div>
        </div>
      </div>

      <div class="submenu-column center"></div>
      <div class="submenu-column right"></div>
    </div>
  </header>
</template>

<script>
import { walletAPI, CardanoWasm, balanceTx } from "@/api/wallet-api";

export default {
  setup() {
    const wallet = walletAPI();

    return {
      wallet,
    };
  },
  data() {
    return {
      isScrolled: false,
      currentRoute: "",
      navTabs: [
        {
          label: "All",
          value: "all",
          badge: false,
          badgeLabel: "",
        },
        {
          label: "New",
          value: "new",
          badge: false,
          badgeLabel: "",
        },
        {
          label: "Offers",
          value: "offers",
          badge: false,
          badgeLabel: "",
        },
        {
          label: "Docs",
          value: "docs",
          badge: false,
          badgeLabel: "",
        },
        {
          label: "Bounties",
          value: "bounties",
          badge: false,
          badgeLabel: "",
        },
        {
          label: "Help",
          value: "help",
          badge: false,
          badgeLabel: "",
        },
      ],
    };
  },
  created() {
    this.$watch(
      () => this.$route.name,
      (name) => (this.currentRoute = name),
      { immediate: true }
    )();
  },
  methods: {
    connectWallet() {
      this.wallet.connect("nami");
    },

    async getPubKeyHash() {
      const usedAddr = await window.cardano.getUsedAddresses();

      const addrMap = usedAddr.map((hexAddr) => {
        let byteAddr = CardanoWasm.Address.from_hex(hexAddr);
        // eslint-disable-next-line
        let pkh = CardanoWasm.BaseAddress.from_address(byteAddr)
          .payment_cred()
          .to_keyhash()
          .to_hex();

        return {
          address: byteAddr.to_bech32(),
        };
      });

      const contractAddr = "-";

      const contractAdd = CardanoWasm.Address.from_bech32(contractAddr);

      const contractPkh = CardanoWasm.BaseAddress.from_address(contractAdd)
        .payment_cred()
        .to_keyhash()
        .to_hex();

      console.log(JSON.stringify(addrMap));
      console.log("contractAddr", JSON.stringify(contractPkh));
    },

    async deploy() {
      const tx = "-";

      const result = await balanceTx(tx);
      console.log(result);
    },
  },
  mounted() {
    window.addEventListener("scroll", () => {
      if (window.scrollY > 0) {
        this.isScrolled = true;
      } else {
        this.isScrolled = false;
      }
    });
  },
};
</script>

<style lang="css" scoped>
.wallet-button {
  font-size: var(--text-size-a);
  border-radius: 999px;
  padding: 0.5rem;
  color: var(--text-a);
  background: var(--base-a);
  border: 1px solid var(--border-b);
  font-weight: 500;
  display: flex;
  align-items: center;
  cursor: pointer;
}

.wallet-button img {
  margin-left: 1rem;
}

.submenu {
  padding: 0.125rem;
  padding-left: 2rem;
  margin-top: 7px;
  z-index: 100;
  display: flex;
  position: fixed;
  top: 70px;
  left: 0;
  width: 100%;
  align-items: center;
  background: var(--base-a);
  color: var(--text-a);
  border-top: 1px solid var(--border-a);
  border-bottom: 1px solid var(--border-a);
  font-weight: 500;
}

.submenu.blue {
  color: var(--text-w);
  background: var(--blue-c);
  font-weight: 600;
  border-bottom: 1px solid transparent;
}

.submenu .submenu-column {
  flex-basis: 33.33%;
}

.submenu .submenu-column.left {
  flex-basis: 33.33%;
  display: flex;
  justify-content: flex-start;
  align-items: center;
}

.submenu .submenu-column.right {
  flex-basis: 33.33%;
  display: flex;
  justify-content: center;
}

.submenu .submenu-column.center {
  flex-basis: 66.66%;
  width: auto;
  display: flex;
  align-items: center;
  justify-content: center;
}

.submenu .submenu-column .submenu-nav {
  display: flex;
  align-items: center;
}

.submenu .submenu-column .submenu-button {
  cursor: pointer;
  margin-right: 1rem;
}

.submenu .submenu-column .submenu-button img {
  width: var(--text-size-e);
}

.submenu .submenu-column .submenu-nav div {
  font-size: var(--text-size-a);
  white-space: nowrap;
  cursor: pointer;
  padding: 0 1rem;
  line-height: 40px;
  font-weight: inherit;
  margin-right: 1rem;
  color: inherit;
  background: transparent;
  border-radius: 999px;
}

.submenu .submenu-column .submenu-nav div:hover {
  opacity: 0.8;
}

.header {
  padding: 1rem 2rem;
  display: flex;
  align-items: center;
  justify-content: space-between;
  position: fixed;
  top: 0;
  z-index: 1000;
  width: 100%;
  box-sizing: border-box;
  background: initial;
  color: var(--text-a);
  background: white;
  box-shadow: var(--border-shadow);
}

.header .header-left {
  flex-basis: 33.33%;
  display: flex;
  justify-content: flex-start;
  align-items: center;
}

.header .header-left .header-left-logo {
  cursor: pointer;
  image-rendering: optimizeQuality;
}

.header-button {
  font-weight: 600;
  font-size: var(--text-size-d);
  display: flex;
  align-items: center;
  white-space: nowrap;
  cursor: pointer;
  transition: var(--button-transition-a);
}

.header-button.left {
  margin-right: auto;
}

.header-button.right {
  margin-left: auto;
}

.header .header-center {
  flex-basis: 66.66%;
  width: auto;
  display: flex;
  align-items: center;
  justify-content: center;
}

.header-center-search {
  background: var(--base-b);
  transition: var(--button-transition-a);
  cursor: text;
  display: flex;
  align-items: center;
  border-radius: 999px;
  width: 100%;
  color: var(--text-a);
  font-size: var(--text-size-b);
  padding: 0.25rem;
  border: 1px solid var(--border-b);
}

.header-center-search:focus-within {
  background: var(--base-a);
  border: 1px solid var(--border-b);
}

.header-center-search-icon {
  color: var(--text-w);
  font-size: var(--text-size-b);
  display: flex;
  align-items: center;
  padding: 1rem;
  cursor: pointer;
  background: var(--blue-c);
  border-radius: 999px;
}

.header-center-search-icon i {
  font-weight: bold;
}

.header-center-search input {
  background-color: transparent;
  border: none;
  outline: none;
  width: 100%;
  font-size: var(--text-size-b);
  color: inherit;
  margin: 0 1rem;
  padding: 0.5rem;
}

.header-center-search input::placeholder {
  color: inherit;
  font-weight: 400;
  opacity: 0.6;
}

.header .header-right {
  flex-basis: 33.33%;
  display: flex;
  justify-content: flex-end;
}

i {
  line-height: 0;
}

.header-button div {
  display: flex;
  flex-direction: column;
  margin-left: 1rem;
}

.header-button div:hover {
  transition: var(--button-transition-a);
}

.header-button span {
  font-size: var(--text-size-b);
  line-height: 1.25rem;
  text-align: left;
}

.header-button span:nth-child(1) {
  font-weight: 400;
}

.header-button img {
  width: var(--text-size-h);
  height: var(--text-size-h);
}

.header-button label {
  width: 2rem;
  height: 2rem;
  border-radius: 3px;
  display: flex;
  justify-content: center;
  align-items: center;
  cursor: pointer;
}

.header-button i {
  width: 2rem;
}

.counter {
  background: var(--blue-c);
  color: var(--text-w);
  font-weight: 600 !important;
  border-radius: 99px;
  text-align: center !important;
  font-size: var(--text-size-a) !important;
}


@media only screen and (max-width: 600px) {
  header {
    padding-left: 1rem;
    padding-right: 1rem;
  }
  .mobile {
    display: none !important;
  }

  .header-left {
    justify-content: space-between;
  }
}
</style>
