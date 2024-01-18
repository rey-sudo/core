<template>
  <header class="header">
    <div class="header-left">
      <img
        class="header-left-logo"
        @click="reloadPage"
        src="@/assets/logo.svg"
        alt=""
      />
      <div class="header-left-counter">
        <div>4</div>
        <img src="@/assets/mobile-car.svg" alt="" />
      </div>
    </div>

    <div class="header-center">
      <div class="header-center-search">
        <input type="text" maxlength="200" placeholder="Search products" />

        <div class="header-center-search-icon">
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
  border-radius: 8px;
  width: 100%;
  color: var(--text-a);
  font-size: var(--text-size-a);
  padding: 0.5rem;
  border: 1px solid #dddddd;
  background: var(--base-b);
  margin: 0.5rem 0;
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
  font-weight: bold;
}

.header-center-search input {
  background-color: transparent;
  border: none;
  outline: none;
  width: 100%;
  font-size: var(--text-size-a);
  color: inherit;
  margin: 0 0.5rem;
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
  transition: var(--transition-a);
}

.border:hover {
  box-shadow: 0 3px 12px 0 rgba(0, 0, 0, 0.1), 0 1px 2px 0 rgba(0, 0, 0, 0.08);
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

.header-button i {
  width: 2rem;
}
.header-left-counter {
  position: relative;
  justify-content: center;
  display: flex;
  align-items: center;
  width: 2.5rem;
  height: 2.5rem;
  margin-left: auto;
}
.header-left-counter div {
  color: var(--blue-a);
  font-weight: bold;
  display: flex;
  justify-content: center;
  text-align: start;
  font-size: var(--text-size-b);
  position: absolute;
  top: -3px;
  margin-left: 3px;
  margin-bottom: 4px;
  width: inherit;
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
