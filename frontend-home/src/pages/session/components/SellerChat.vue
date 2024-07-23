<template>
  <div class="chat">
    <div class="chat-top">
      <img :src="getCurrentSeller?.avatar" alt="" />

      <span>{{ getCurrentSeller?.username }}</span>
    </div>
    <div class="chat-body"></div>
    <div class="chat-bottom"></div>
  </div>
</template>

<script>
import headerAPI from "@/components/header/composable/header-api";

export default {
  setup() {
    const { getCurrentSeller } = headerAPI();

    return {
      getCurrentSeller,
    };
  },
};
</script>

<style lang="css" scoped>
.chat {
  background: var(--base-a);
  width: 400px;
  height: 700px;
  border-radius: 16px;
  margin-left: auto;
  border: 3px solid var(--border-a);
  display: flex;
  flex-direction: column;
  overflow: hidden;
}

.chat-body {
  height: 100%;
  background: var(--base-a);
  overflow-y: scroll;
  overflow-x: hidden;
}

.chat-bottom {
  height: 100px;
}

.chat-top {
  height: 100px;
  padding: 1rem;
  display: flex;
  align-items: center;
  border-bottom: 1px solid var(--border-a);
}

.chat-top img {
  border-radius: 50%;
  width: 40px;
  height: 40px;
  border: 1px solid var(--border-a);
  background: var(--base-a);
}

.chat-top span {
  margin-left: 1rem;
  font-weight: 500;
}
</style>
