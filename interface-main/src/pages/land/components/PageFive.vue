<template>
  <div class="p5" ref="observed5">
    <div class="p5-wrap">
      <div class="p5-wrap-container">
        <div class="p5-wrap-container-top">
          {{ getter__productData.theme.config.page_5.emoji }}
        </div>

        <div class="p5-wrap-container-title">
          {{ getter__productData.theme.config.page_5.title }}
        </div>

        <div class="p5-wrap-container-subtitle">
          {{ getter__productData.theme.config.page_5.subtitle }}
        </div>
      </div>

      <div id="cobeWrap" />
    </div>
  </div>
</template>

<script>
import createGlobe from "cobe";
import locations from "@/pages/land/utils/locations";
import landAPI from "@/pages/land/composable/land-api";

export default {
  setup() {
    const { getter__productData } = landAPI();

    return {
      getter__productData,
    };
  },
  mounted() {
    let phi = 0;
    let color = [0.4, 0.4, 1];

    const observer = new IntersectionObserver(
      (entries) => {
        entries.forEach((entry) => {
          if (entry.isIntersecting) {
            let canvas = document.createElement("canvas");
            canvas.id = "cobeCanvas";
            canvas.width = 1000;
            canvas.height = 1000;
            canvas.style.width = "1000px";
            canvas.style.height = "1000px";

            const cobeWrap = document.getElementById("cobeWrap");

            cobeWrap.appendChild(canvas);

            this.T1 = setTimeout(
              () =>
                createGlobe(canvas, {
                  devicePixelRatio: 2,
                  width: 1000 * 2,
                  height: 1000 * 2,
                  phi: 0,
                  theta: 0,
                  dark: 1,
                  diffuse: 4,
                  mapSamples: 16000,
                  mapBrightness: 1,
                  baseColor: color,
                  markerColor: color,
                  glowColor: color,
                  opacity: 0,
                  markers: locations,
                  onRender: (state) => {
                    state.phi = phi;
                    phi += 0.001;
                  },
                }),
              600
            );
          } else {
            clearTimeout(this.T1);

            let canvas = document.getElementById("cobeWrap");
            if (canvas.firstChild) {
              canvas.firstChild.remove();
            }
          }
        });
      },
      { root: null, rootMargin: "0px", threshold: 0.5 }
    );

    observer.observe(this.$refs.observed5);
  },
};
</script>

<style lang="css" scoped>
::v-deep(canvas) {
  top: 50%;
  left: 0;
  position: relative;
  right: 0;
  z-index: 1;
}

.p5 {
  display: flex;
  justify-content: center;
  height: 100%;
  overflow: hidden;
  background: black;
}

.p5 .p5-wrap {
  width: 90%;
  height: 100%;
  display: flex;
  justify-content: center;
  align-items: center;
  flex-direction: column;
}

.p5-wrap-container {
  position: absolute;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  top: 20vh;
  z-index: 2;
}

.p5 .p5-wrap .p5-wrap-container-top {
  font-size: var(--text-size-e);
  font-weight: 400;
  align-items: center;
  border-radius: 50%;
  justify-content: center;
  display: flex;
}

.p5 .p5-wrap .p5-wrap-container-title {
  font-size: var(--text-size-f);
  font-weight: 600;
  color: var(--text-a);
  letter-spacing: -0.03em;
  color: var(--text-w);
}

.p5 .p5-wrap .p5-wrap-container-subtitle {
  font-size: var(--text-size-c);
  font-weight: 400;
  max-width: 50%;
  color: var(--text-w);
  margin-top: 1rem;
}

@media only screen and (max-width: 768px) {
  .p5 .p5-wrap .p5-wrap-container-title {
    font-size: var(--text-size-e);
  }

  .p5 .p5-wrap .p5-wrap-container-subtitle {
    font-size: var(--text-size-b);
    padding: 0 1rem;
    max-width: initial;
  }
}
</style>
