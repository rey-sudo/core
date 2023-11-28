<template>
  <div class="store">
    <div class="store-top">
      <MainHeader />
      <div class="store-top-mask" />

      <div class="store-top-menu">
        <div class="store-top-menu-col left">
          <div class="store-top-menu-button">
            <img src="@/assets/menu.svg" alt="" />
          </div>

          <div class="store-top-menu-nav">
            <div
              v-for="item in navTabs"
              :key="item"
              :class="{ active: selectedTab === item.value }"
            >
              {{ item.label }}
            </div>
          </div>
        </div>

        <div class="store-top-menu-col center"></div>
        <div class="store-top-menu-col right"></div>
      </div>

      <div class="store-top-banner"></div>
    </div>

    <GridWrap />
  </div>
</template>

<script>
import MainHeader from "@/components/MainHeader.vue";
import GridWrap from "@/pages/store/components/GridWrap.vue";
import storeAPI from "@/pages/store/composable/store-api";
import { useRouter } from "vue-router";

export default {
  components: {
    MainHeader,
    GridWrap,
  },

  setup() {
    const router = useRouter();

    const { action__getAllProducts } = storeAPI();

    action__getAllProducts()
      .then(() => {})
      .catch((err) => console.error(err));

    return { router, action__getAllProducts };
  },
  data() {
    return {
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
          label: "Support",
          value: "support",
          badge: false,
          badgeLabel: "",
        },
      ],
    };
  },
};
</script>

<style lang="css" scoped>


.store {
  width: 100%;
  height: 100vh;
  display: flex;
  flex-direction: column;
  background: var(--secondary-b);
}

.store-top {
  width: inherit;
  height: 600px;
  min-height: 600px;
  background: var(--base-a);
  z-index: 1;
  position: relative;
  inset: 0px 0px -1px;
  background-size: contain;
  display: flex;
  flex-direction: column;
}

.store-top-banner {
  display: grid;
  grid-template-columns: 1fr 1fr;
  grid-gap: 2rem;
  z-index: 100;
  padding: 0 4rem;
  margin-top: 2rem;
}

.store-top-mask {
  width: inherit;
  height: inherit;
  z-index: 2;
  position: absolute;
  backdrop-filter: blur(0px);
  background: linear-gradient(180deg, var(--secondary-c) 80%, transparent 100%);
  background: var(--secondary-b);
}

.store-top-menu {
  padding-left: 4rem;
  margin-top: 74px;
  z-index: 100;
  display: flex;
  align-items: center;
  background: var(--secondary-a);
  border-top: 1px solid rgb(255, 255, 255, 0.1);
}

.store-top-menu-nav {
  display: flex;
  align-items: center;
  padding: 0.25rem;
}

.store-top-menu-button {
  cursor: pointer;
  margin-right: 1rem;
}

.store-top-menu-button img {
  width: var(--text-size-d);
}

.store-top-menu-nav div {
  font-size: var(--text-size-a);
  white-space: nowrap;
  cursor: pointer;
  padding: 0 1rem;
  line-height: 40px;
  font-weight: 400;
  margin-right: 1rem;
  color: var(--text-w);
  background: transparent;
  border-radius: 999px;
}

.store-top-menu-nav div:hover {
  color: var(--text-w);
  background: rgba(255, 255, 255, 0.1);
}

.store-top-menu-col {
  flex-basis: 33.33%;
}

.store-top-menu-col.left {
  flex-basis: 33.33%;
  display: flex;
  justify-content: flex-start;
  align-items: center;
}

.store-top-menu-col.right {
  flex-basis: 33.33%;
  display: flex;
  justify-content: center;
}

.store-top-menu-col.center {
  flex-basis: 66.66%;
  width: auto;
  display: flex;
  align-items: center;
  justify-content: center;
}
</style>
