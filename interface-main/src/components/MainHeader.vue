<template>
  <header class="header" :class="{ scrolled: isScrolled }">
    <div class="header-column left">
      <img class="header-logo" :class="{ scrolled: isScrolled }" @click="reloadPage" src="@/assets/logo-white.svg"
        alt="" />

      <div class="header-box left" :class="{ scrolled: isScrolled }">
        <label>🇺🇸</label>
        <div>
          <span>EN /</span>
          <span>USA</span>
        </div>
      </div>

      <div class="header-box left" :class="{ scrolled: isScrolled }">
        <label></label>
        <div>
          <span>Hi, Sell</span>
          <span>Products</span>
        </div>
      </div>

    </div>

    <div class="header-column center">
      <div class="header-search" :class="{ scrolled: isScrolled }">
        <input :class="{ scrolled: isScrolled }" type="text" maxlength="200"
          placeholder="Buscar productos, marcas y más..." />

        <div><i class="pi pi-search" /></div>
      </div>
    </div>

    <div class="header-column right">
      <div class="header-box right" :class="{ scrolled: isScrolled }">
        <label></label>
        <div>
          <span>Or, Audit</span>
          <span>Products</span>
        </div>
      </div>

      <div class="header-box right" :class="{ scrolled: isScrolled }">
        <img src="@/assets/car.svg" alt="">
        <div>
          <span class="counter">0</span>
          <span>Items</span>
        </div>
      </div>

      <div class="header-box right" :class="{ scrolled: isScrolled }">
        <img src="@/assets/wallet.svg" alt="">
        <div>
          <span>Connect</span>
          <span>Wallet</span>
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

.header.scrolled {
  background: var(--base-a);
  border-bottom: 1px solid var(--border-a);
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
  margin: 0 auto;
}

.header-box.right {
  margin: 0 auto;
}

.header-box div {
  display: flex;
  flex-direction: column;
  margin-left: 1rem;
}

.header-box span {
  font-size: var(--text-size-a);
  line-height: 1.25rem;
  text-align: left;
}

.header-box span:nth-child(1) {
  font-weight: 400;
}

.header-box img,
label {
  width: var(--text-size-e);
}

.counter {
  background: rgba(255, 255, 255, 1);
  color: var(--text-a);
  border-radius: 99px;
  text-align: center !important;
  font-size: 12px !important;
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
  border: 1px solid var(--blue);
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
</style>
