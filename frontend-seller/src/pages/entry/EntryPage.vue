<template>
  <div class="entry">
    <RegisterView />
  </div>
</template>

<script>
import RegisterView from "@/pages/entry/components/RegisterView";
export default {
  components: {
    RegisterView,
  },
};
</script>

<style lang="css" scoped>
.entry {
  display: flex;
  align-items: center;
  justify-content: center;
  background: var(--blue-a);
  height: 100vh;
}

</style>
