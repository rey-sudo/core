<template>
  <div class="p4">
    <div class="p4-wrap">
      <div class="overlay">
        <div class="p4-wrap-top">🤯</div>

        <div class="p4-wrap-title">¡Multiples beneficios!</div>

        <div class="p4-wrap-subtitle">
          <p>
            ¿Por qué perder más tiempo cortando manualmente cuando puedes
            hacerlo de manera rápida y precisa con nuestro picador?
          </p>
        </div>

        <div class="p4-wrap-grid">
          <div
            class="p4-wrap-grid-item"
            :style="{
              backgroundImage: `url(https://arkastore.nyc3.digitaloceanspaces.com/test/1200x1200-1.webp)`,
            }"
          >
            <div class="content">
              <div>Seguridad</div>
              <div>
                Superficie de corte con rieles y sujetador de mano para más
                seguridad.
              </div>
            </div>
            <span class="gradient" />
          </div>
          <div
            class="p4-wrap-grid-item"
            :style="{
              backgroundImage: `url(https://arkastore.nyc3.digitaloceanspaces.com/test/1200x1200-10.webp)`,
            }"
          >
            <div class="content">
              <div>Eficiencia</div>
              <div>Los cortes se realizan en menos de un segundo.</div>
            </div>
            <span class="gradient" />
          </div>
          <div
            class="p4-wrap-grid-item"
            :style="{
              backgroundImage: `url(https://arkastore.nyc3.digitaloceanspaces.com/test/1200x1200-14.webp)`,
            }"
          >
            <div class="content">
              <div>Versatilidad</div>
              <div>
                Las 7 cuchillas de corte permiten gran variedad de resultados.
              </div>
            </div>
            <span class="gradient" />
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
export default {};
</script>

<style lang="css" scoped>
.p4 {
  display: flex;
  justify-content: center;
  height: 100%;
}

.p4 .p4-wrap {
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
}

.p4 .p4-wrap .p4-wrap-top {
  font-size: var(--text-size-g);
  font-weight: 400;
  align-items: center;
  border-radius: 50%;
  justify-content: center;
  display: flex;
}

.p4 .p4-wrap .p4-wrap-title {
  font-size: var(--text-size-f);
  font-weight: 600;
  line-height: 82px;
  color: var(--text-a);
  letter-spacing: -0.03em;
}

.p4 .p4-wrap .p4-wrap-subtitle {
  font-size: var(--text-size-b);
  font-weight: 400;
  color: var(--text-b);

  max-width: 50%;
  line-height: 24px;
}

.p4 .p4-wrap .p4-wrap-grid {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  grid-gap: 2rem;
  margin-top: 1rem;
}

.p4 .p4-wrap .p4-wrap-grid .p4-wrap-grid-item {
  width: 400px;
  height: 600px;
  background: var(--base-a);
  transition: 0.2s;
  background-clip: content-box;
  display: flex;
  flex-direction: column;
  text-align: center;
  box-sizing: border-box;
  position: relative;
  border-radius: 0px;
  align-items: center;
  justify-content: flex-end;
  line-height: 24px;
  overflow: hidden;
  background-repeat: no-repeat;
  background-size: cover;
  background-position: center;
  border-radius: 17px;
}

.p4 .p4-wrap .p4-wrap-grid .p4-wrap-grid-item:hover {
  animation: slideAnimation 4s infinite linear;
}

@keyframes slideAnimation {
  0% {
    background-position: 0 0;
  }
  100% {
    background-position: 100% 0;
  }
}

.gradient {
  width: inherit;
  height: inherit;
  position: absolute;
  z-index: 1;
  top: 0;
  left: 0;
  background: linear-gradient(
    to top,
    rgba(51, 51, 51, 1) 0%,
    rgba(0, 0, 0, 0) 100%
  );
}

.p4 .p4-wrap .p4-wrap-grid .p4-wrap-grid-item div {
  color: var(--text-w);
}

.content {
  z-index: 2;
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
  margin-bottom: 2rem;
  padding: 1rem;
}
.content div:nth-child(1) {
  display: flex;
  align-items: center;
  justify-content: flex-start;
  font-size: var(--text-size-d);
  font-weight: 500;
  letter-spacing: -0.03em;
}

.content div:nth-child(2) {
  font-size: var(--text-size-b);
  font-weight: 400;
  letter-spacing: -0.03em;
  margin-top: 1rem;
}
</style>
