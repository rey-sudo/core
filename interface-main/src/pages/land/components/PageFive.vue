<template>
  <div class="page5">
    <div class="page5-wrap">
      <div class="page5-wrap-container">
        <div class="page5-wrap-container-top">
          {{ getter__productData.theme.config.page_5.emoji }}
        </div>

        <div class="page5-wrap-container-title">
          {{ getter__productData.theme.config.page_5.title }}
        </div>

        <div class="page5-wrap-container-subtitle">
          {{ getter__productData.theme.config.page_5.subtitle }}
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


.page5 {
  display: flex;
  justify-content: center;
  height: 100vh;
  min-height: 100vh;
  width: 100%;
  overflow: hidden;
  background: black;
}

.page5 .page5-wrap {
  width: inherit;
  height: inherit;
  display: flex;
  justify-content: center;
  align-items: center;
  flex-direction: column;
}

.page5-wrap-container {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  top: 20vh;
  z-index: 2;
}

.page5 .page5-wrap .page5-wrap-container-top {
  font-size: var(--text-size-e);
  font-weight: 400;
  align-items: center;
  border-radius: 50%;
  justify-content: center;
  display: flex;
}

.page5 .page5-wrap .page5-wrap-container-title {
  font-size: var(--text-size-f);
  font-weight: 600;
  color: var(--text-a);
  letter-spacing: -0.03em;
  color: var(--text-w);
}

.page5 .page5-wrap .page5-wrap-container-subtitle {
  font-size: var(--text-size-c);
  font-weight: 400;
  max-width: 50%;
  color: var(--text-w);
  margin-top: 1rem;
  letter-spacing: -0.01em;
}

@media only screen and (max-width: 768px) {
  .page5 .page5-wrap .page5-wrap-container-title {
    font-size: var(--text-size-e);
  }

  .page5 .page5-wrap .page5-wrap-container-subtitle {
    font-size: var(--text-size-b);
    padding: 0 1rem;
    max-width: initial;
    line-height: 24px;
  }
}
</style>
