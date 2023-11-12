<template>
  <header class="p-header" :class="{ scrolled: isScrolled }">
    <div class="p-header-column left">
      <img class="p-header-logo" :class="{ scrolled: isScrolled }" @click="reloadPage" src="@/assets/logo-white.svg"
        alt="" />

      <div class="p-header-nav">
        <div>{{ labels.sellLabel }}</div>
        <div>{{ labels.helpLabel }}</div>
      </div>
    </div>

    <div class="p-header-column center">
      <div class="p-header-search" :class="{ scrolled: isScrolled }">
        <input :class="{ scrolled: isScrolled }" type="text" maxlength="200"
          placeholder="Buscar productos, marcas y más..." />

        <div><i class="pi pi-search" /></div>
      </div>
    </div>

    <div class="p-header-column right">
      <div class="p-header-button" :class="{ scrolled: isScrolled }">
        <img src="@/assets/car.svg" alt="">
        <div>
          <span class="counter">0</span>
          <span>Items</span>
        </div>
      </div>

      <div class="p-header-button" :class="{ scrolled: isScrolled }">
        <img src="@/assets/wallet.svg" alt="">
        <div>
          <span>Connect</span>
          <span>wallet</span>
        </div>
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
        sellLabel: "",
        helpLabel: "",
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



.p-header-search.scrolled {
  border: 1px solid var(--border-a);
  color: var(--text-b);
}

.p-header-search.scrolled:focus-within {
  border: 1px solid var(--blue);
  color: var(--text-b);
}

.p-header-search.scrolled:hover {
  border: 1px solid var(--blue);
  color: var(--text-b);
}

.p-header .p-header-logo {
  cursor: pointer;
  height: var(--text-size-e);
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

.p-header-button {
  font-weight: 600;
  font-size: var(--text-size-c);
  display: flex;
  align-items: center;
  white-space: nowrap;
  cursor: pointer;
  transition: var(--button-transition-a);
  margin-left: 2rem;
}
.p-header-button div {
  display: flex;
  flex-direction: column;
  margin-left: 1rem;
}

.p-header-button span {
  font-size: var(--text-size-a);
  line-height: 1.25rem;
}
.p-header-button span:nth-child(1) {
  font-weight: 400;
}
.p-header-button span:nth-child(2) {
  
}

.p-header-button img {
  width: var(--text-size-e);
}

.counter {
  background: rgba(255, 255, 255, 1);
  color: var(--text-a);
  border-radius: 99px;
  text-align: center;
  font-size: 12px !important;
}

.p-header-search {
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

.p-header-search:hover {
  border: 1px solid var(-blue);
}

.p-header-search:focus-within {
  border: 1px solid var(--blue);
}

.p-header-search div {
  background: black;
  color: var(--text-w);
  font-size: var(--text-size-b);
  border-radius: 999px;
  display: flex;
  align-items: center;
  padding: 1rem;
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
  color: inherit;
  margin: 0 1rem;
}

.p-header-search input::placeholder {
  color: inherit;
  font-weight: 400;
  opacity: 0.6;
}
</style>
