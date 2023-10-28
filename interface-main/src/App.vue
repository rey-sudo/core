<template>
  <main>
    <router-view />
  </main>
</template>

<script>
export default {
  name: "App",
};
</script>

<style>
#app {
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: var(--text-a);
  background: var(--base-a);
  font-family: Inter, sans-serif;
  text-shadow: 0.5px 0.5px 1px rgba(0, 0, 0, 0.1);
}

i {
  line-height: 0 !important;
}

a {
  text-decoration: none;
}

body {
  margin: 0;
}

:root {
  --text-size-a: 14px;
  --text-size-b: 17.5px;
  --text-size-c: 21.88px;
  --text-size-d: 27.34px;
  --text-size-e: 34.18px;
  --text-size-f: 42.72px;
  --text-size-g: 53.41px;
  --text-size-h: 66.76px;

  --violet-a: #8a97ff;
  --violet-b: rgba(138, 151, 255, 0.5);
  --violet-c: rgba(138, 151, 255, 0.9);

  --text-a: #1d1d1f;
  --text-b: #6e6e73;
  --text-c: #06c;
  --text-w: #ffffff;

  --base-a: #ffffff;
  --base-b: #fbfbfd;
  --base-c: #f2f2f2;
  --base-d: #fafafa;

  --blue: #0071e3;
  --green: #27b46a;

  --secondary-a: #131722;
  --secondary-b: #2d313c;
  --secondary-c: #e3f1ff;
  --secondary-d: #c7e3ff;
  --secondary-e: #ffffff;

  --glass-a: rgba(3, 0, 20, 0.8);

  --soft-a: rgba(3, 0, 20, 0.5);

  --button-transition-a: background-color 0.2s cubic-bezier(0.05, 0, 0.2, 1) 0s,
    border-color 0.2s cubic-bezier(0.05, 0, 0.2, 1) 0s;

  --button-shadow: inset 0 0 12px rgba(138, 151, 255, 0.5);

  --button-background: linear-gradient(
      180deg,
      rgba(79, 11, 168, 0) 0%,
      rgba(138, 151, 255, 0.24) 100%
    ),
    rgba(138, 151, 255, 0.24);

  --shadow-a: 0 5px 40px rgba(0, 0, 0, 0.1);
  --shadow-b: 0 2px 3px -1px rgba(0, 0, 0, 0.2), 0 0 1px 0 rgba(0, 0, 0, 0.14),
    0 1px 3px 0 rgba(0, 0, 0, 0.12);
  --shadow-c: rgba(0, 0, 0, 0.08) 0px 4px 16px;
  --shadow-d: rgba(0, 0, 0, 0.1) 0px 4px 16px;

  --border-a: rgba(210, 210, 215, 1);
  --border-b: rgba(210, 210, 215, 0.3);
}

.green {
  color: #27b46a;
}

.blue {
  color: blue;
}

.red {
  color: red;
}

.gray {
  color: var(--text-b);
}

ul {
  margin: initial;
}
</style>
