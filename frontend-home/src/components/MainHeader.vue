<template>
  <header class="header">
    <div class="header-left">
      <img
        class="header-left-logo"
        @click="reloadPage"
        src="@/assets/logo.svg"
        alt=""
      />

      <div class="header-button left mobile">
        <label>🇺🇸</label>
        <div>
          <span>EN</span>
          <span>USA</span>
        </div>
      </div>

      <div class="header-button left mobile">
        <label>
          <i class="pi pi-tag" />
        </label>
        <div>
          <span>Hi, Sell</span>
          <span>Product</span>
        </div>
      </div>
    </div>

    <div class="header-center mobile">
      <div class="header-center-search">
        <input type="text" maxlength="200" placeholder="Search products" />

        <div class="header-center-search-icon">
          <i class="pi pi-search" />
        </div>
      </div>
    </div>

    <div class="header-right mobile">
      <div class="header-button right">
        <label> <i class="pi pi-gift"></i></label>
        <div>
          <span>Be a</span>
          <span>Mediator</span>
        </div>
      </div>

      <div class="header-button right">
        <label for=""> <img src="@/assets/car.svg" alt="" /></label>

        <div>
          <span class="counter">14</span>
          <span>Items</span>
        </div>
      </div>

      <div class="header-button right border">
        <label for=""> <i class="pi pi-user" /></label>
        <div @click="connectWallet">
          <span>User</span>
          <span>Wallet</span>
        </div>
      </div>
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
    };
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
.header {
  padding: 1rem;
  padding-left: 1%;
  padding-right: 1%;
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
  height: 38px;
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
  margin: auto;
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
  border: 1px solid transparent;
  background: var(--base-a);
  transition: var(--button-transition-a);
  cursor: text;
  display: flex;
  align-items: center;
  border-radius: 999px;
  width: 100%;
  color: var(--text-a);
  font-size: var(--text-size-a);
  padding: 0.5rem;
  border: 1px solid #dddddd;
  box-shadow: var(--shadow-a);
}

.header-center-search-icon {
  background: var(--blue-a);
  color: var(--text-w);
  font-size: var(--text-size-b);
  border-radius: 999px;
  display: flex;
  align-items: center;
  padding: 1rem;
  cursor: pointer;
}

.header-center-search-icon i {
  font-weight: bold !important;
}

.header-center-search input {
  background-color: transparent;
  border: none;
  outline: none;
  width: 100%;
  font-size: var(--text-size-a);
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

/*------*/
i {
  line-height: 0;
}

.border {
  cursor: pointer;
  border-radius: 32px;
  padding: 0.25rem 1rem;
  border: 1px solid var(--border-a);
}

.border:hover {
  box-shadow: 0 3px 12px 0 rgba(0, 0, 0, 0.1), 0 1px 2px 0 rgba(0, 0, 0, 0.08);
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
  font-size: var(--text-size-a);
  line-height: 1.25rem;
  text-align: left;
}

.header-button span:nth-child(1) {
  font-weight: 400;
}

.header-button img {
  width: 2rem;
  height: 2rem;
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
  background: black;
  color: var(--text-w);
  font-weight: 500 !important;
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
