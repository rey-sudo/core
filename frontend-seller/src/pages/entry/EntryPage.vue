<template>
  <div class="entry">
    <LoginView />
  </div>
</template>

<script>
import LoginView from "@/pages/entry/components/LoginView";
export default {
  components: {
    LoginView,
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
