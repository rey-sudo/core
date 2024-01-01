<template>
  <header class="header" :class="{ scrolled: isScrolled }">
    <div class="header-column left">
      <img
        class="header-logo"
        :class="{ scrolled: isScrolled }"
        @click="reloadPage"
        src="@/assets/logo-white.svg"
        alt=""
      />

      <div class="header-box left hide" :class="{ scrolled: isScrolled }">
        <label>🇺🇸</label>
        <div>
          <span>EN</span>
          <span>USA</span>
        </div>
      </div>

      <div class="header-box left hide" :class="{ scrolled: isScrolled }">
        <label>
          <i class="pi pi-tag" />
        </label>
        <div @click="doBalance">
          <span>Hi,Sell</span>
          <span>Product</span>
        </div>
      </div>
    </div>

    <div class="header-column center hide">
      <div class="header-search" :class="{ scrolled: isScrolled }">
        <input
          :class="{ scrolled: isScrolled }"
          type="text"
          maxlength="200"
          placeholder="Search products"
        />

        <div><i class="pi pi-search" /></div>
      </div>
    </div>

    <div class="header-column right hide">
      <div class="header-box right" :class="{ scrolled: isScrolled }">
        <label> <i class="pi pi-gift"></i></label>
        <div @click="startTx">
          <span>Be a</span>
          <span>Mediator</span>
        </div>
      </div>

      <div class="header-box right" :class="{ scrolled: isScrolled }">
        <label for="">
          <img src="@/assets/car.svg" :class="{ scrolled: isScrolled }" alt=""
        /></label>

        <div>
          <span class="counter" :class="{ scrolled: isScrolled }">0</span>
          <span>Items</span>
        </div>
      </div>

      <div class="header-box right" :class="{ scrolled: isScrolled }">
        <label for=""> <i class="pi pi-wallet" /></label>
        <div @click="connectWallet">
          <span>Login</span>
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
      labels: {
        sellLabel: "",
        helpLabel: "",
        walletLabel: "Billetera",
      },
    };
  },
  methods: {
    connectWallet() {
      this.wallet.connect("nami");
    },

    async startTx() {
      // const tx =
      //  "84a300800181a300581d7055743d5cfd66af33d0891dc9dba441bce20d632b0fe81b0b6cfe483e011a004c4b40028201d818585cd8799f004777616974696e67d87980d87980d87980581c484ebc54b4112e54e1f7524dbdc6bb42635648a06c297e584592e80b581c3f2ec097f77e4254df012d5d4d4b45e48459c6ec5795e92df30f2dbc1a009896801a004c4b40ff0200a0f5f6";

      const addr = CardanoWasm.Address.from_bech32(
        "addr_test1qpqx3nnj5rmnapg0rxye5y9c9mznff26dkrquhzjvlw29wfrsvx98a6ugq6gn7hrversz52hf4yft4af5v9dv78p5zwseyadst"
      );

      const cAddress = CardanoWasm.BaseAddress.from_address(addr)
        .payment_cred()
        .to_keyhash();

      //const result = await window.cardano.getUtxos();

      if (cAddress == null) throw new Error();

      const credential = cAddress;

      const pubKeyHash = credential.to_keyhash(credential);

      console.log(Buffer.from(pubKeyHash).toString("hex"));
    },

    async doBalance() {
      const tx =
        "84a300800181a300581d70814f9750a738594ebcc97becb07b62f3cf5bd9f770ec6ccc152a6f14011a00b71b00028201d818585cd8799f004777616974696e67d87980d87980d87980581c484ebc54b4112e54e1f7524dbdc6bb42635648a06c297e584592e80b581c3f2ec097f77e4254df012d5d4d4b45e48459c6ec5795e92df30f2dbc1a01c9c3801a00b71b00ff0200a0f5f6";
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
header {
  padding: 1.25rem;
  padding-left: 3rem;
  padding-right: 3rem;
  display: flex;
  align-items: center;
  justify-content: space-between;
  position: fixed;
  top: 0;
  z-index: 1000;
  width: 100%;
  box-sizing: border-box;
  background: initial;
  color: var(--text-w);
  background: black;
}

i {
  line-height: 0;
}

img.scrolled {
  filter: invert(1);
}
.header.scrolled {
  background: var(--base-a);
  color: var(--text-a);
  border-bottom: 1px solid var(--border-a);
}
.counter.scrolled {
  background: var(--base-c);
}
.counter.scrolled {
  color: var(--text-a);
}

.header-logo.scrolled {
  filter: invert(1);
}

.header-search.scrolled {
  border: 1px solid var(--border-a);
  color: var(--text-b);
}

.header-search.scrolled:focus-within {
  border: 1px solid var(--blue);
  color: var(--text-b);
}

.header-search.scrolled:hover {
  border: 1px solid var(--blue);
  color: var(--text-b);
}

.header .header-logo {
  cursor: pointer;
  height: var(--text-size-e);
}

.header-column {
  flex-basis: 33.33%;
}

.header-column.left {
  flex-basis: 33.33%;
  display: flex;
  justify-content: flex-start;
  align-items: center;
}

.header-column.right {
  flex-basis: 33.33%;
  display: flex;
  justify-content: flex-end;
}

.header-column.center {
  flex-basis: 66.66%;
  width: auto;
  display: flex;
  align-items: center;
  justify-content: center;
}

.header-nav {
  display: flex;
  align-items: center;
  width: 100%;
  justify-content: center;
}

.header-nav div {
  padding: 0 1rem;
  font-weight: 600;
  font-size: var(--text-size-a);
  cursor: pointer;
  transition: var(--button-transition-a);
}

.header-nav div:hover {
  opacity: 0.7;
}

.header-box {
  font-weight: 600;
  font-size: var(--text-size-c);
  display: flex;
  align-items: center;
  white-space: nowrap;
  cursor: pointer;
  transition: var(--button-transition-a);
}

.header-box.left {
  margin: auto;
}

.header-box.right {
  margin: 0 auto;
}

.header-box div {
  display: flex;
  flex-direction: column;
  margin-left: 1rem;
}

.header-box div:hover {
  text-decoration: underline;
  transition: var(--button-transition-a);
}

.header-box span {
  font-size: var(--text-size-a);
  line-height: 1.25rem;
  text-align: left;
}

.header-box span:nth-child(1) {
  font-weight: 400;
}

.header-box img {
  width: 2rem;
  height: 2rem;
}

.header-box label {
  width: 2rem;
  height: 2rem;
  border-radius: 3px;
  display: flex;
  justify-content: center;
  align-items: center;
}

.header-box i {
  width: 2rem;
}

.counter {
  background: white;
  color: var(--text-a);
  font-weight: 700 !important;
  border-radius: 99px;
  text-align: center !important;
  font-size: var(--text-size-a) !important;
}

.header-search {
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
  padding: 0.25rem;
}

.header-search:hover {
  border: 1px solid var(-blue);
}

.header-search:focus-within {
  border: 1px solid transparent;
}

.header-search div {
  background: black;
  color: var(--text-w);
  font-size: var(--text-size-b);
  border-radius: 999px;
  display: flex;
  align-items: center;
  padding: 1rem;
  cursor: pointer;
}

.header-search div i {
  font-weight: bold !important;
}

.header-search input {
  background-color: transparent;
  border: none;
  outline: none;
  width: 100%;
  font-size: var(--text-size-a);
  color: inherit;
  margin: 0 1rem;
}

.header-search input::placeholder {
  color: inherit;
  font-weight: 400;
  opacity: 0.6;
}

@media only screen and (max-width: 600px) {
  header {
    padding-left: 1rem;
    padding-right: 1rem;
  }
  .hide {
    display: none !important;
  }

  .header-column.left {
    justify-content: space-between;
  }
}
</style>
