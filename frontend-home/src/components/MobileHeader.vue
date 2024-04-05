<template>
  <header class="header">
    <div class="header-left">
      <img
        class="header-left-logo"
        @click="reloadPage"
        src="@/assets/logo.svg"
        alt=""
      />
      <div class="header-left-item">
        <i class="pi pi-user" />
      </div>
      <div class="header-left-item right">
        <i class="pi pi-shopping-cart" />
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

    <div class="header-right">
      <div class="header-right-nav">
        <div class="header-right-nav-button">
          <i class="pi pi-bars" />
        </div>

        <div class="header-right-nav-item">
          <div
            v-for="item in navTabs"
            :key="item"
            :class="{ active: selectedTab === item.value }"
          >
            {{ item.label }}
          </div>
        </div>
      </div>
    </div>
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
      selectedTab: "all",
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
          label: "Help",
          value: "help",
          badge: false,
          badgeLabel: "",
        },
      ],
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
  padding: 0 0.75rem;
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
  justify-content: space-between;
  align-items: center;
  height: 50px;
}

.header .header-left .header-left-logo {
  cursor: pointer;
  height: 38px;
}

.header .header-left .header-left-item {
  position: relative;
  justify-content: center;
  display: flex;
  align-items: center;
  margin-left: auto;
  font-size: var(--text-size-e);
  width: 2rem;
  height: 2rem;
}

.header .header-left .header-left-item.right {
  margin-left: 1rem;
}

.header .header-center {
  flex-basis: 66.66%;
  width: auto;
  display: flex;
  align-items: center;
  justify-content: center;
  height: 50px;
}

.header .header-center .header-center-search {
  transition: var(--button-transition-a);
  cursor: text;
  display: flex;
  align-items: center;
  border-radius: 999px;
  width: 100%;
  color: var(--text-a);
  font-size: var(--text-size-a);
  border: 3px solid transparent;
  background: var(--base-a);
}

.header .header-center .header-center-search:focus-within {
  border: 3px solid var(--border-a);
}

.header .header-center .header-center-search div {
  background: transparent;
  color: var(--text-a);
  border-radius: 8px;
  display: flex;
  align-items: center;
  padding: 1.25rem 1rem;
  cursor: pointer;
  margin-right: -1px;
}

.header .header-center .header-center-search div i {
  font-size: var(--text-size-c);
}

.header .header-center .header-center-search input {
  background-color: transparent;
  border: none;
  outline: none;
  width: 100%;
  font-size: var(--text-size-b);
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
  justify-content: flex-start;
  height: 50px;
}

.header .header-right .header-right-nav {
  display: flex;
  align-items: center;
  width: 100%;
}

.header .header-right .header-right-nav-button {
  cursor: pointer;
  line-height: 0;
}

.header .header-right .header-right-nav-button img {
  width: var(--text-size-e);
}

.header .header-right .header-right-nav-item {
  display: flex;
  align-items: center;
  width: inherit;
}

.header .header-right .header-right-nav-item div {
  font-size: var(--text-size-b);
  white-space: nowrap;
  cursor: pointer;
  font-weight: 600;
  margin: auto;
  color: var(--text-a);
  background: transparent;
  border-radius: 999px;
}

.header .header-right .header-right-nav-item div:hover {
  color: var(--text-a);
  background: var(--base-b);
}
@media only screen and (max-width: 600px) {
  .header {
    display: block;
  }

  .header-left {
    justify-content: space-between;
  }
}
</style>
