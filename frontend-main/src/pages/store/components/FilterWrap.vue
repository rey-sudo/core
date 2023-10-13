<template>
  <div class="p-filter">
    <div class="p-filter-left">
      <span
        class="slider"
        :class="{
          left: switchValue === 'trending',
          right: switchValue === 'top',
        }"
      />

      <span
        :class="{ active: switchValue === 'trending' }"
        @click="handleSwitch('trending')"
        >Trending</span
      >

      <span
        :class="{ active: switchValue === 'top' }"
        @click="handleSwitch('top')"
        >Top</span
      >
    </div>
  </div>
</template>

<script>
export default {
  data() {
    return {
      switchValue: "trending",
    };
  },
  methods: {
    handleSwitch(e) {
      this.switchValue = e;
    },
  },
};
</script>

<style lang="css" scoped>
.active {
  color: var(--text-a);
}

.p-filter {
  z-index: 100;
  display: flex;
  align-items: center;
  justify-content: space-between;
  padding: 0 4rem;
  margin-top: 1rem;
  margin-bottom: 1rem;
  font-size: var(--text-size-a);
 
}

.p-filter .p-filter-left {
  border-radius: 10px;
  background: rgba(18, 18, 18, 0.04);
  background: var(--base-c);
  display: flex;
  align-items: center;
}

span {
  width: 125px;
  height: 52px;
  display: flex;
  align-items: center;
  justify-content: center;
  color: var(--text-b);
  font-weight: 600;
  z-index: 2;
  position: relative;
  cursor: pointer;
}

.slider {
  width: 125px;
  height: 48px;
  margin: 4px;
  border-radius: 10px;
  color: var(--text-a);
  font-weight: 600;
  position: absolute;
  z-index: 1 !important;
  background: var(--base-a);
  transition: all 0.3s ease 0s;
}

.slider.left {
  transform: translateX(0px);
}

.slider.right {
  transform: translateX(117px);
}
</style>
