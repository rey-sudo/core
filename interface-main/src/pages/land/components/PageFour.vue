<template>
  <div class="p4">
    <div class="p4-wrap">
      <div class="overlay">
        <div class="p4-wrap-top">🤯</div>

        <div class="p4-wrap-title">¡Multiples beneficios!</div>

        <div class="p4-wrap-subtitle">
          <p>
            The community can easily identify in the dapp store which projects
            are the most secure according to 3 levels of audit.
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
              <div>In-depth analysis of the fundamental aspects</div>
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
              <div>In-depth analysis of the fundamental aspects</div>
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
              <div>In-depth analysis of the fundamental aspects</div>
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
  background: var(--base-c);
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
  font-size: var(--text-size-c);
  font-weight: 400;
  align-items: center;
  border-radius: 50%;
  box-shadow: var(--button-shadow);
  width: 45px;
  height: 45px;
  justify-content: center;
  display: flex;
}

.p4 .p4-wrap .p4-wrap-title {
  font-size: var(--text-size-g);
  font-weight: 500;
  line-height: 82px;
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  -webkit-background-clip: text;
  background-clip: text;
  background-image: linear-gradient(
    90deg,
    #1e3791 0%,
    #2948b1 40%,
    #3153c6 55%,
    #385fda 60%
  );
  color: transparent;
  letter-spacing: -0.03em;
  margin: 12px auto 0;
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
  background-position: 50%;

}

.p4 .p4-wrap .p4-wrap-grid .p4-wrap-grid-item:hover {
  padding: 1rem;
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
