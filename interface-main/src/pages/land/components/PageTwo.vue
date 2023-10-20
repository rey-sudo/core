<template>
  <div
    class="p-pageTwo"
    :style="{
      background: getter__productData.theme.config.page_2.background_color,
    }"
  >

  <div class="backgroundMask">
      <img :src="getter__productData.theme.config.page_2.mask" />
    </div>


    <div class="p-pageTwo-wrap">
      <div class="p-pageTwo-wrap-grid">
        <div class="p-pageTwo-wrap-grid-item">
          <div><img src="@/pages/land/assets/certification.svg" alt="" /></div>
          <div>Certification</div>
          <div>Three different levels of audit depth with certification</div>
          <span />
        </div>
        <div class="p-pageTwo-wrap-grid-item">
          <div><img src="@/pages/land/assets/sandbox.svg" alt="" /></div>
          <div>Sandbox</div>
          <div>Multipurpose test environment for auditing</div>
          <span />
        </div>
        <div class="p-pageTwo-wrap-grid-item">
          <div><img src="@/pages/land/assets/governance.svg" alt="" /></div>
          <div>Governance</div>
          <div>The community makes fundamental decisions</div>
          <span />
        </div>
        <div class="p-pageTwo-wrap-grid-item">
          <div><img src="@/pages/land/assets/formal.svg" alt="" /></div>
          <div>Formal methods</div>
          <div>Specification and verification of smart contracts</div>
          <span />
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import landAPI from "@/pages/land/composable/land-api";

export default {
  setup(){
    const { getter__productData } = landAPI();
    
    return{
      getter__productData 
    }
  }
};
</script>

<style lang="css" scoped>
.backgroundMask {
  position: absolute;
  width: inherit;
  height: inherit;
}

.backgroundMask img {
  width: inherit;
  height: inherit;
  object-fit: cover;
  object-position: bottom;
}

.p-pageTwo {
  display: flex;
  justify-content: center;
  height: 100%;
  background: var(--base-a);
}

.p-pageTwo-wrap {
  width: 90%;
  height: 100%;
  border-bottom: none;
  display: flex;
  justify-content: center;
  align-items: center;
}

.p-pageTwo .p-pageTwo-wrap .p-pageTwo-wrap-grid {
  display: grid;
  grid-template-columns: repeat(2, 1fr);
  grid-gap: 1rem;
}

.p-pageTwo .p-pageTwo-wrap .p-pageTwo-wrap-grid .p-pageTwo-wrap-grid-item {
  padding: 1.75rem;
  width: 600px;
  text-align: center;
  height: 400px;
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
  background: var(--base-d);
  border-radius: 12px;
}

.p-pageTwo .p-pageTwo-wrap .p-pageTwo-wrap-grid .p-pageTwo-wrap-grid-item img {
  width: 42px;
}

.p-pageTwo .p-pageTwo-wrap .p-pageTwo-wrap-grid .p-pageTwo-wrap-grid-item span {
  width: 20px;
  height: inherit;
  position: absolute;
  z-index: 2;
  top: 0;
  left: calc(50% - 10px);
  border-top: 1px solid transparent;
  border-bottom: 1px solid var(--border-b);
}

.p-pageTwo
  .p-pageTwo-wrap
  .p-pageTwo-wrap-grid
  .p-pageTwo-wrap-grid-item
  div:nth-child(1) {
  display: flex;
  align-items: center;
  justify-content: flex-start;
}

.p-pageTwo
  .p-pageTwo-wrap
  .p-pageTwo-wrap-grid
  .p-pageTwo-wrap-grid-item
  div:nth-child(2) {
  color: var(--text-a);
  font-weight: 500;
  margin-top: 1rem;
}

.p-pageTwo
  .p-pageTwo-wrap
  .p-pageTwo-wrap-grid
  .p-pageTwo-wrap-grid-item
  div:nth-child(3) {
  color: var(--text-b);
  margin-top: 1rem;
}
</style>
