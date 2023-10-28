<template>
  <div class="p-mobile">
    <img
      :src="
        getter__productData.space_url +
        getter__productData.theme.config.page_1.images[0].url
      "
      alt=""
    />
  </div>
</template>

<script>
import landAPI from "@/pages/land/composable/land-api";

export default {
  setup() {
    const { getter__productData } = landAPI();

    return {
      getter__productData,
    };
  },
};
</script>

<style lang="css" scoped>
.p-mobile {
  width: inherit;
  height: 300px;
  display: none;
}

.p-mobile img {
  width: calc(100% - 2rem);
  height: 300px;
  border-radius: 34px;
}

@media only screen and (max-width: 768px) {
  .p-mobile {
    display: initial;
  }
}
</style>
