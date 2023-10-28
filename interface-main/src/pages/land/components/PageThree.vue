<template>
  <div
    class="page3"
    :style="{
      background: getter__productData.theme.config.page_3.background_color,
    }"
  >
    <div class="mask">
      <img
        :src="
          getter__productData.space_url +
          getter__productData.theme.config.page_3.mask
        "
      />
    </div>

    <div class="page3-wrap">
      <div class="page3-wrap-grid">
        <div class="page3-wrap-grid-item">
          <div>
            {{ getter__productData.theme.config.page_3.section.content.title }}
          </div>
          <div>
            <p>
              {{ getter__productData.theme.config.page_3.section.content.text }}
            </p>
          </div>
          <span />
        </div>

        <div class="page3-wrap-grid-item">
          <img
            :src="
              getter__productData.space_url +
              getter__productData.theme.config.page_3.section.image
            "
            alt=""
          />
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
.page3 {
  display: flex;
  justify-content: center;
  width: 100%;
  height: 100vh;
  min-height: 100vh;
  background: var(--base-a);
}

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

.page3-wrap {
  width: 100%;
  height: 100%;
  border-bottom: none;
  display: flex;
  justify-content: center;
  align-items: center;
}

.page3 .page3-wrap .page3-wrap-grid {
  display: grid;
  grid-template-columns: repeat(2, 1fr);
  grid-gap: 1rem;
}

.page3 .page3-wrap .page3-wrap-grid .page3-wrap-grid-item {
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

.page3 .page3-wrap .page3-wrap-grid .page3-wrap-grid-item p {
  text-align: center;
}

.page3 .page3-wrap .page3-wrap-grid .page3-wrap-grid-item img {
  width: inherit;
  object-fit: cover;
  position: relative;
}

.page3 .page3-wrap .page3-wrap-grid .page3-wrap-grid-item span {
  width: 20px;
  height: inherit;
  position: absolute;
  z-index: 2;
  top: 0;
  left: calc(50% - 10px);
  border-top: 1px solid transparent;
  border-bottom: 1px solid var(--border-b);
}

.page3 .page3-wrap .page3-wrap-grid .page3-wrap-grid-item div:nth-child(1) {
  font-size: var(--text-size-e);
  font-weight: 600;
  text-align: center;
  letter-spacing: -0.03em;
}

.page3 .page3-wrap .page3-wrap-grid .page3-wrap-grid-item div:nth-child(2) {
  font-size: var(--text-size-c);
  margin-top: 1rem;
  text-align: center;
  max-width: 80%;
  font-weight: 400;
}

@media only screen and (max-width: 768px) {
  .page3 .page3-wrap .page3-wrap-grid {
    display: grid;
    grid-template-columns: repeat(1, 1fr);
    grid-gap: 1rem;
    padding: 1rem;
  }

  .page3 .page3-wrap .page3-wrap-grid .page3-wrap-grid-item {
    width: 100%;
    height: 300px;
  }
}
</style>
