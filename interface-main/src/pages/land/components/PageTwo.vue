<template>
  <div
    class="p2"
    :style="{
      background: getter__productData.theme.config.page_2.background_color,
    }"
  >
    <div class="mask">
      <img
        :src="
          getter__productData.space_url +
          getter__productData.theme.config.page_2.mask
        "
      />
    </div>

    <div class="p2-wrap">
      <div class="p2-wrap-grid">
        <div class="p2-wrap-grid-item">
          <img
            :src="
              getter__productData.space_url +
              getter__productData.theme.config.page_2.box.image
            "
            alt=""
          />
        </div>
        <div class="p2-wrap-grid-item">
          <div>
            {{ getter__productData.theme.config.page_2.box.content.title }}
          </div>
          <div>
            <p>
              {{ getter__productData.theme.config.page_2.box.content.text }}
            </p>
          </div>
          <span />
        </div>
      </div>
    </div>
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
.mask {
  position: absolute;
  width: inherit;
  height: inherit;
}

.mask img {
  width: inherit;
  height: inherit;
  object-fit: cover;
  object-position: bottom;
}

.p2 {
  display: flex;
  justify-content: center;
  height: 100%;
  background: var(--base-a);
}

.p2-wrap {
  width: 100%;
  height: 100%;
  border-bottom: none;
  display: flex;
  justify-content: center;
  align-items: center;
}

.p2 .p2-wrap .p2-wrap-grid {
  display: grid;
  grid-template-columns: repeat(2, 1fr);
  grid-gap: 1rem;
}

.p2 .p2-wrap .p2-wrap-grid .p2-wrap-grid-item {
  width: 600px;
  text-align: center;
  height: 600px;
  background-clip: content-box;
  border-top: transparent;
  border-bottom: transparent;
  display: flex;
  flex-direction: column;
  text-align: start;
  justify-content: center;
  box-sizing: border-box;
  position: relative;
  line-height: 24px;
  background: var(--base-c);
  border-radius: 34px;
  overflow: hidden;
  color: var(--text-a);
  align-items: center;
}

.p2 .p2-wrap .p2-wrap-grid .p2-wrap-grid-item p {
  text-align: center;
}

.p2 .p2-wrap .p2-wrap-grid .p2-wrap-grid-item img {
  width: inherit;
  object-fit: cover;
  position: relative;
}

.p2 .p2-wrap .p2-wrap-grid .p2-wrap-grid-item span {
  width: 20px;
  height: inherit;
  position: absolute;
  z-index: 2;
  top: 0;
  left: calc(50% - 10px);
  border-top: 1px solid transparent;
  border-bottom: 1px solid var(--border-b);
}

.p2 .p2-wrap .p2-wrap-grid .p2-wrap-grid-item div:nth-child(1) {
  font-size: var(--text-size-e);
  font-weight: 600;
  text-align: center;
  letter-spacing: -0.03em;
}

.p2 .p2-wrap .p2-wrap-grid .p2-wrap-grid-item div:nth-child(2) {
  font-size: var(--text-size-c);
  margin-top: 1rem;
  text-align: center;
  max-width: 80%;
  font-weight: 500;
}

@media only screen and (max-width: 768px) {
  .p2 .p2-wrap .p2-wrap-grid {
    display: grid;
    grid-template-columns: repeat(1, 1fr);
    grid-gap: 1rem;
    padding: 1rem;
  }

  .p2 .p2-wrap .p2-wrap-grid .p2-wrap-grid-item {
    width: 100%;
    height: 300px;
  }
}
</style>
