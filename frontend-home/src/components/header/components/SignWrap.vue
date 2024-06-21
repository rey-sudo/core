<template>
  <Sidebar
    v-model:visible="visible"
    header="Sign in with Cardano wallet"
    position="right"
    :blockScroll="true"
  >
    <p>
      Select a Cardano wallet to enter pairfy.
    </p>
  </Sidebar>
</template>

<script>
import { ref } from "vue";

export default {
  setup() {
    const visible = ref(true);
    return { visible };
  },
};
</script>

<style lang="css" scoped></style>
