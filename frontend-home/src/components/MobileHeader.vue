<template>
  <header class="header">
    <div class="header-left">
      <img
        class="header-left-logo"
        @click="reloadPage"
        src="@/assets/logo.svg"
        alt=""
      />
      <div class="header-left-count">
        <div>4</div>
        <img src="@/assets/mobile-car.svg" alt="" />
      </div>
    </div>

    <div class="header-center">
      <div class="header-center-search">
        <input type="text" maxlength="200" placeholder="Search products" />

        <div>
          <i class="pi pi-search" />
        </div>
      </div>
    </div>

    <div class="header-right"></div>
  </header>
</template>

<script>
import { walletAPI } from "@/api/wallet-api";

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
i {
  line-height: 0;
}
.header {
  padding: 0.5rem;
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
  display: none;
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

.header .header-left .header-left-count {
  position: relative;
  justify-content: center;
  display: flex;
  align-items: center;
  width: 2.25rem;
  height: 2.25rem;
  margin-left: auto;
}
.header .header-left .header-left-count div {
  color: var(--text-a);
  font-weight: bold;
  display: flex;
  justify-content: center;
  text-align: start;
  font-size: var(--text-size-b);
  position: absolute;
  top: -4px;
  margin-left: 3px;
  width: inherit;
}

.header .header-center {
  flex-basis: 66.66%;
  width: auto;
  display: flex;
  align-items: center;
  justify-content: center;
}

.header .header-center .header-center-search {
  border: 1px solid transparent;
  background: var(--base-a);
  transition: var(--button-transition-a);
  cursor: text;
  display: flex;
  align-items: center;
  border-radius: 8px;
  width: 100%;
  color: var(--text-a);
  font-size: var(--text-size-a);
  padding: 0.5rem;
  border: 1px solid #dddddd;
  background: var(--base-b);
  margin: 0.5rem 0;
}

.header .header-center .header-center-search div {
  background: var(--blue-a);
  color: var(--text-w);
  font-size: var(--text-size-b);
  border-radius: 999px;
  display: flex;
  align-items: center;
  padding: 1rem;
  cursor: pointer;
}

.header .header-center .header-center-search div i {
  font-weight: bold;
}

.header .header-center .header-center-search input {
  background-color: transparent;
  border: none;
  outline: none;
  width: 100%;
  font-size: var(--text-size-a);
  color: inherit;
  margin: 0 0.5rem;
  padding: 0.5rem;
}

.header .header-center .header-center-search input::placeholder {
  color: inherit;
  font-weight: 400;
  opacity: 0.6;
}

.header .header-right {
  flex-basis: 33.33%;
  display: flex;
  justify-content: flex-end;
}

@media only screen and (max-width: 600px) {
  .header {
    padding-left: 1rem;
    padding-right: 1rem;
    display: block;
  }

  .header-left {
    justify-content: space-between;
  }
}
</style>
