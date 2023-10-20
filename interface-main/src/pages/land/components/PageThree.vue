<template>
  <div class="p-pageThree"
  :style="{ background: getter__productData.theme.background.pageThree.color }"
  
  >
    <div class="backgroundMask">
      <img :src="getter__productData.theme.background.pageThree.mask" />
    </div>

    <div class="p-pageThree-wrap">
      <div class="overlay">
        <video autoplay src="../assets/video2.mp4" />
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

video {
  width: 100%;
  height: auto;
  max-width: 680px;
  max-height: 600px;
}
.p-pageThree {
  display: flex;
  justify-content: center;
  width: inherit;
  height: inherit;
  background-repeat: no-repeat;
  background-size: cover;
  background-image: url("../assets/background-blow-green.svg");
}

.p-pageThree .p-pageThree-wrap {
  width: 90%;
  height: 100%;
  display: flex;
  justify-content: center;
  align-items: center;
  flex-direction: column;
}

.overlay {
  position: absolute;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  background: var(--base-d);
  height: 600px;
  width: 1200px;
  border-radius: 12px;
}
</style>
