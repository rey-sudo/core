<template>
  <div class="p-store">
    <div class="p-store-top">
      <MainHeader />

      <div class="p-store-top-mask" />

      <div class="p-store-submenu">
        <div class="p-header-column left">
          <div class="p-store-submenu-nav">
            <div
              v-for="item in navTabs"
              :key="item"
              :class="{ active: selectedTab === item.value }"
            >
              {{ item.label }}
            </div>
          </div>
        </div>
        <div class="p-header-column center"></div>

        <div class="p-header-column right"></div>
      </div>

      <div class="p-store-banner">
        <div class="p-store-banner-item">
          <img :src="banner1" />
        </div>
        <div class="p-store-banner-item">
          <img :src="banner2" />
        </div>
      </div>
    </div>

    <FilterWrap />
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
      .then((res) =>
        router.push({
          name: "land",
          params: { pid: res.response[0].items[0].pid },
        })
      )
      .catch((err) => console.error(err));

    return { router, action__getAllProducts };
  },
  data() {
    return {
      selectedTab: "all",
      navTabs: [
        {
          label: "Todo",
          value: "all",
          badge: false,
          badgeLabel: "",
        },
        {
          label: "Nuevo",
          value: "new",
          badge: false,
          badgeLabel: "",
        },
        {
          label: "Ofertas",
          value: "offers",
          badge: false,
          badgeLabel: "",
        },
        {
          label: "Categorías",
          value: "categories",
          badge: false,
          badgeLabel: "",
        },
        {
          label: "Tecnología",
          value: "tech",
          badge: false,
          badgeLabel: "",
        },
      ],
      banner1:
        "https://http2.mlstatic.com/D_NQ_612942-MLA71945575734_092023-OO.webp",
      banner2:
        "https://http2.mlstatic.com/D_NQ_662196-MLA72063692845_102023-OO.webp",
    };
  },
};
</script>

<style lang="css" scoped>
.p-store-banner {
  display: grid;
  grid-template-columns: 1fr 1fr;
  grid-gap: 2rem;
  z-index: 100;
  padding: 0 4rem;
  margin-top: 2rem;
}

.p-store-banner-item {
  border-radius: 12px;
  background: var(--base-b);
  height: 300px;
  gap: 2rem;
  flex: 1;
  min-width: 50%;
  max-width: 100%;
  padding: 0;
  box-sizing: border-box;
  overflow: hidden;
  display: flex;
  align-items: center;
  justify-content: center;
}

.p-store-banner-item img {
  display: block;
  height: 100%;
  object-fit: cover;
  object-position: top;
  width: 100%;
  object-position: 40%;
  opacity: 0;
}

.p-store {
  width: 100%;
  height: 100vh;
  display: flex;
  flex-direction: column;
}

.p-store-top {
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

.p-store-top-mask {
  width: inherit;
  height: inherit;
  z-index: 2;
  position: absolute;
  backdrop-filter: blur(0px);
  background: linear-gradient(180deg, var(--secondary-c) 80%, transparent 100%);
  background: var(--base-a);
}

.p-store-submenu {
  padding-left: 4rem;
  margin-top: 74px;
  z-index: 100;
  display: flex;
  align-items: center;
  background: var(--secondary-a);
}

.p-store-submenu-nav {
  display: flex;
  align-items: center;
}
.p-store-submenu-nav div {
  font-size: var(--text-size-a);
  white-space: nowrap;
  cursor: pointer;
  padding: 1rem 0;
  margin-right: 2rem;
  color: var(--text-w);
}

.p-store-submenu-nav div:hover {
  color: var(--blue);
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
  justify-content: center;
}

.p-header-column.center {
  flex-basis: 66.66%;
  width: auto;
  display: flex;
  align-items: center;
  justify-content: center;
}
</style>
