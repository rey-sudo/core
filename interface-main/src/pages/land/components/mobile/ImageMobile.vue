<template>
  <div class="p-mobile">
<img src="/products/123456789/images/p123456789-a5-w1200-h1200.webp" alt="">
  </div>
</template>

<script>
export default {};
</script>

<style lang="css" scoped>
.p-mobile {
  width: 300px;
  height: 300px;
  display: none;
}

.p-mobile img {
  width: 300px;
  height: 300px;
}

@media only screen and (max-width: 768px) {
  .p-mobile {
display: initial;
  }
}
</style>
