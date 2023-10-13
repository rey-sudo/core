<template>
  <header class="p-header" :class="{ scrolled: isScrolled }">
    <div class="p-header-column left">
      <img
        class="p-header-logo"
        :class="{ scrolled: isScrolled }"
        @click="reloadPage"
        src="@/assets/logo-white.svg"
        alt=""
      />
      <div class="p-header-divider" :class="{ scrolled: isScrolled }" />

      <div class="p-header-nav">
        <div>{{ labels.sellLabel }}</div>
        <div>{{ labels.helpLabel }}</div>
      </div>
    </div>

    <div class="p-header-column center">
      <div class="p-header-search" :class="{ scrolled: isScrolled }">
        <input
          :class="{ scrolled: isScrolled }"
          type="text"
          maxlength="200"
          placeholder="Buscar productos, marcas y más..."
        />

        <div><i class="pi pi-search" /></div>
      </div>
    </div>

    <div class="p-header-column right">
      <div class="p-header-wallet" :class="{ scrolled: isScrolled }">
        <div><i class="pi pi-wallet" /></div>

        <div class="p-header-divider" :class="{ scrolled: isScrolled }" />

        <span>{{ labels.walletLabel }}</span>
      </div>

      <div class="p-header-config" :class="{ scrolled: isScrolled }">
        <i class="pi pi-cog" />
      </div>
    </div>
  </header>
</template>

<script>
export default {
  data() {
    return {
      isScrolled: false,
      labels: {
        sellLabel: "Vender",
        helpLabel: "Soporte",
        walletLabel: "Billetera",
      },
    };
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
  padding: 1rem;
  padding-left: 4rem;
  padding-right: 4rem;
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
  background: var(--secondary-a);
}

.p-header.scrolled {
  background: var(--base-a);
  border-bottom: 1px solid var(--border-a);
  color: var(--text-a);
}

.p-header-logo.scrolled {
  filter: invert(1);
}

.p-header-divider.scrolled {
  background: var(--border-a);
}

.p-header-wallet.scrolled {
  background: inherit;
  border: 1px solid var(--border-a);
}

.p-header-wallet.scrolled:hover {
  border: 1px solid var(--secondary-d);
}

.p-header-config.scrolled {
  background: inherit;
  border: 1px solid var(--border-a);
}

.p-header-config.scrolled:hover {
  border: 1px solid var(--secondary-d);
}

.p-header-search.scrolled {
  border: 1px solid var(--border-a);
  color: var(--text-b);
  padding: 0.5rem;
}

.p-header-search.scrolled:focus-within {
  border: 1px solid var(--secondary-d);
  color: var(--text-b);
}

.p-header-search.scrolled:hover {
  border: 1px solid var(--secondary-d);
  color: var(--text-b);
}

.p-header .p-header-logo {
  cursor: pointer;
  height: var(--text-size-e);
}

.p-header-divider {
  height: 20px;
  width: 1px;
  margin: 0 1rem;
  background: rgba(255, 255, 255, 0.25);
}

.p-header-column {
  flex-basis: 33.33%;
}

.p-header-column.left {
  flex-basis: 33.33%;
  display: flex;
  justify-content: flex-start;
  align-items: center;
}

.p-header-column.right {
  flex-basis: 33.33%;
  display: flex;
  justify-content: flex-end;
}

.p-header-column.center {
  flex-basis: 66.66%;
  width: auto;
  display: flex;
  align-items: center;
  justify-content: center;
}
.p-header-nav {
  display: flex;
  align-items: center;
  width: 100%;
  justify-content: center;
}
.p-header-nav div {
  padding: 0 1rem;
  font-weight: 600;
  font-size: var(--text-size-a);
  cursor: pointer;
  transition: var(--button-transition-a);
}

.p-header-nav div:hover {
  opacity: 0.7;
}

.p-header-wallet {
  font-weight: 600;
  font-size: var(--text-size-a);
  border-radius: 999px;
  padding: 0.75rem 1rem;
  display: flex;
  align-items: center;
  white-space: nowrap;
  cursor: pointer;
  border: 1px solid transparent;
  transition: var(--button-transition-a);
}

.p-header-config {
  font-weight: 600;
  font-size: var(--text-size-a);
  border-radius: 999px;
  padding: 0.75rem 1rem;
  background: var(--secondary-b);
  display: flex;
  align-items: center;
  white-space: nowrap;
  cursor: pointer;
  border: transparent;
  border: 1px solid transparent;
  transition: var(--button-transition-a);
  margin-left: 1rem;
}

.p-header-config:hover {
  background-color: rgba(255, 255, 255, 0.2);
}
.p-header-wallet:hover {
  background-color: rgba(255, 255, 255, 0.2);
}

.p-header-search {
  line-height: 26px;
  border: 3px solid transparent;
  background: var(--base-a);
  transition: var(--button-transition-a);
  cursor: text;
  display: flex;
  align-items: center;
  border-radius: 999px;
  width: 100%;
  color: inherit;
  color: var(--text-a);
  font-size: var(--text-size-a);
  padding: calc(0.5rem - 2px);
}

.p-header-search:hover {
  border: 3px solid var(--secondary-d);
}

.p-header-search:focus-within {
  border: 3px solid var(--secondary-d);
}

.p-header-search div {
  height: 28px;
  background: black;
  color: white;
  border-radius: 999px;
  display: flex;
  align-items: center;
  padding: 0 1rem;
  cursor: pointer;
}

.p-header-search div i {
  font-weight: bold !important;
}

.p-header-search input {
  background-color: transparent;
  border: none;
  outline: none;
  width: 100%;
  font-size: var(--text-size-a);
  line-height: 26px;
  min-height: 26px;
  color: inherit;
  margin: 0 1rem;
}

.p-header-search input::placeholder {
  color: inherit;
  font-weight: 400;
  opacity: 0.6;
}
</style>
