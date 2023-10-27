<template>
  <div class="p5" ref="observed5">
    <div class="p5-wrap">
      <div class="p5-wrap-container">
        <div class="p5-wrap-container-top">🇨🇴</div>

        <div class="p5-wrap-container-title">Envíos gratis a toda Colombia</div>

        <div class="p5-wrap-container-subtitle">
          Enviamos a todas las ciudades de Colombia: ¡Tu destino está a solo un
          clic de distancia! Realizamos entregas rápidas y seguras a cada rincón
          del país. ¡Haz tu pedido ahora y paga en casa!
        </div>
      </div>

      <div id="cobeWrap" />
    </div>
  </div>
</template>

<script>
import createGlobe from "cobe";
import locations from "@/pages/land/utils/locations";

export default {
  mounted() {
    let phi = 0;

    let canvas = document.createElement("canvas");
    canvas.id = "cobeCanvas";

    canvas.style.width = "1000px";
    canvas.style.height = "1000px";

    const cobeWrap = document.getElementById("cobeWrap");

    cobeWrap.appendChild(canvas);

    createGlobe(canvas, {
      devicePixelRatio: 2,
      width: 1000 * 2,
      height: 1000 * 2,
      phi: 0,
      theta: 0,
      dark: 1,
      diffuse: 1.2,
      mapSamples: 16000,
      mapBrightness: 1,
      baseColor: [0.4, 0.4, 1],
      markerColor: [0.4, 0.4, 1],
      glowColor: [0.4, 0.4, 1],
      opacity: 1,
      markers: locations,
      onRender: (state) => {
        state.phi = phi;
        phi += 0.001;
      },
    });
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
  line-height: 82px;
  color: var(--text-a);
  letter-spacing: -0.03em;
  color: var(--text-w);
}

.p5 .p5-wrap .p5-wrap-container-subtitle {
  font-size: var(--text-size-c);
  font-weight: 400;
  max-width: 50%;
  color: var(--text-w);
}
</style>
